---
title: 浅析 emacs-lisp 中的 compiler-macro
date: 2019-09-10
tag:
 - emacs
 - lisp
---

# 什么是 compiler macro

在Elisp里, 我们通常会使用两种构造抽象的方式, macro和function. 这两者的区别相信读
者都已经了然于胸 (如果你不清楚, 建议查阅Elisp Reference Manual).

简言之, compiler macro可以在字节码编译时用特定的规则展开行如`(func arg1 arg2 arg3)`
的函数调用, 宏调用不会被compiler macro展开, 因为你可以在宏体里直接指定代码变换的
方式, 无需借助compiler maro的威力 :​).

注意: 通常来说只有标准形式`(func arg1 arg2)`的函数调用才会被compiler macro打开优
化, 类似`mapcar`或者`apply` `funcall`的高阶函数调用, compiler macro将无能为力

是不是听着很熟悉? 是的, compiler macro可以做到内联优化, 尽管Elisp编译器很笨拙,但
所幸他给我们这些性能狂人留下了很多手动操作的空间.

compiler macro并不能算作与macro和function类比的组织抽象的方式, 只能算作Elisp 为
我们提供的函数优化器, 它没有自己独立的语义, 而是依附于函数而存在.

NOTE:

一个例外是`(funcall #'func 1 2)` 或者`(apply #'func '(1 2 3 4))`, byte-compiler
会消除掉这种`funcall`或者`apply`, 然后交给compiler macro展开.

# 如何察看compiler macro的展开结果

## macroexpand

`macroexpand` `macroexpand-1` `macroexpand-all` 可以打开直接compiler macro.

``` lisp
ELISP> (macroexpand-all '(cadr '(1 3)))
(car
 (cdr
  '(1 3)))
```


这里用`cadr`举例子, cadr可以取出列表中第二个元素, 等效于取列表的`cdr`的`car`.
可以看出来compiler macro直接把cadr展开成`(car (cdr x))`

``` lisp
ELISP> (macroexpand-all '(mapcar #'cadr '((1 2) ((3 4)))))
(mapcar #'cadr
        '((1 2)
          ((3 4))))
```

在这里, `cadr`作为mapcar的参数使用, compiler macro就无能为力了.

## cl-compiler-macroexpand

但是有时候你可能希望不要打开常规宏, 只打开compiler macro, 这时可以使用`cl-lib`中提
供的`cl-compiler-macroexpand`

``` lisp
ELISP> (cl-compiler-macroexpand '(cadr '(1 2)))
(car
 (cdr
  '(1 2)))
```

``` lisp
ELISP> (cl-compiler-macroexpand '(if-let* ((a (cadr '(1 3)))) a))
(if-let*
    ((a
      (cadr
       '(1 3))))
    a)
```

对比

``` lisp
ELISP> (macroexpand-all '(if-let* ((a (cadr '(1 3)))) a))
(let*
    ((a
      (and t
           (car
            (cdr
             '(1 3))))))
  (if a a))
```

## macrostep

[macrostep](https://github.com/joddie/macrostep) 可以展开compiler macro, 在
macrostep里, compiler macro和macro会用不同的face来标记

# 如何编写compiler macro

## compiler macro的定义

compiler-macro的定义和常规宏一样, 是一个返回一个s表达式的lambda, 这个lambda接收
的参数数量视用户调用compiler macro对应的函数时传入的数量而定. 第一个参数固定为要展开的form,
其余的参数依次为用户传入函数调用的参数.

## `compiler-macro` symbol property

为了使compiler macro生效, 你需要将你定义的compiler macro设置为目标函数symbol的
`compiler-macro` property上.

``` lisp
(defalias 'my-list 'list)
(put 'my-list 'compiler-macro
            (lambda (form &rest args) (message "Form: %S, ARGS: %S" form args)
            form))
(cl-compiler-macroexpand '(my-list '(1 2 3 4)))
```

当我们展开compiler macro时, 会得到信息

``` lisp
Form: (my-list 1 2 3 4), ARGS: (1 2 3 4)
```

当你的compiler macro只用于一个函数, 一般可以忽略掉form直接用args.

## declare form

在Emacs 24.4 或更高版本, 你可以直接在函数的`declare` form中指定compiler macro,

详见[manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html)

# 实战

## `my-list*` 函数

考虑函数`my-list*`, 它接受任意数量的参数, 将他们从头用cons连接到尾部.

我们的初版函数是这样的.

``` lisp
(defun my-list* (&rest args)
  (let* ((rargs (reverse args))
         (result (car rargs)))
    (dolist (arg (cdr rargs))
      (push arg result))
    result))
```

``` lisp
(my-list* 1 2 3)                        ;=> '(1 2 . 3)
(my-list* 1 2 '(3))                     ;=> '(1 2 3)
```

我们可以用高阶函数把它变得更简洁

``` lisp
(require 'cl-lib)
(defun my-list* (&rest args)
  (cl-reduce #'cons args :from-end t))
```

你可能已经看出来,`my-list*`等效于手写`(cons arg1 (cons arg2 (cons arg3 ...)))`.
另一方面, 我们都知道打开的循环比循环更高效, 但是我们完全没有必要为了性能而手写展
开形式, 更何况`my-list*`接受任意数量的参数, 根本无法直接手写展开.

这时候可以利用compiler macro生成sexp优化我们的`my-list*` 函数

``` lisp
(put 'my-list* 'compiler-macro
     (lambda (_form &rest args)
       ;; 这里可以用`nreverse', 因为每次调用`(my-list* 1 2 3)'都会生成
       ;; 新的args, 而`(apply #'my-list* something)' 不会触发compiler macro
       (let* ((rargs (nreverse args))
              (head (pop rargs))
              (result head))
         (dolist (arg rargs)
           (setq result `(cons ,arg ,result)))
         result)))
```

来尝试一下

``` lisp
ELISP> (cl-compiler-macroexpand '(my-list* 1 2 3 4 5 6 6 7 8 9))
(cons 1
      (cons 2
            (cons 3
                  (cons 4
                        (cons 5
                              (cons 6
                                    (cons 6
                                          (cons 7
                                                (cons 8 9)))))))))

```

可以看到我们写的compiler macro已经如愿展开了.

## 注意事项

警告: 在compiler macro中, 你可以随意修改函数展开的方式, 如果操作不当, 很可能会导
致直接调用函数与`funcall`调用函数时函数的行为不一致!

``` lisp
(defun my-id (x) x)

(put 'my-id 'compiler-macro
     (lambda (_ arg)
       `(list ,arg)))

;; Compiler macro不会在解释运行时展开, 这里使用`my-id'的原始定义.
(my-id 1)                               ;=> 1

(eval (byte-compile '(my-id 1)))        ;=> (1) ???

(eval (byte-compile '(let ((f #'my-id)) (funcall f 1)))) ;=> 1
```


这里利用funcall规避了compiler macro展开, 由于我们的compiler macro不规范, 导致
`(my-id 1)` 和`(funcall f 1)`造成了不一致的结果, 请使用compiler macro的时候务必
注意, 小心不要造成undefined behaviour.

# 广义变量展开中的compiler macro

由于`macroexpand`可以展开compiler macro, 因此`setf`也会打开广义变量里的
compiler macro

``` lisp
(defun my-aref (arr idx)
  (aref arr idx))

(macroexpand-all '(setf (my-aref arr 1) 3))
;; => (let* ((v arr)) (\(setf\ my-aref\) 3 v 1))

(put 'my-aref 'compiler-macro
     (lambda (_ arr idx)
       `(aref ,arr ,idx)))

(macroexpand-all '(setf (my-aref arr 1) 3))
;; => (let* ((v arr)) (aset v 1 3))
```

我们用`my-aref`简单包裹了`aref`函数, 然而Emacs并不会进入我们的函数定义去查看我们
实际进行的动作,emacs只会去寻找`my-aref`的gv-setter, 并且在找不到的情况下使用了
setter的默认值`(setf my-aref)`(当然这里我们没有定义),
而使用了compiler macro后, 我们给编译器足够的提示, 成功`my-aref`被打开成对应的
`aref` Emacs已经定义了`aref`的gv-setter`aset`, `setf`就可以直接使用aset作为
my-aref的gv-setter

# define-inline

为了更好的利用compiler macro, Emacs 25提供了`inline.el`, 作为compiler的上层包装,
协助用户更好更简单写出安全的内联函数.

## inline-quote

用`define-inline` 定义函数类似于定义一个宏, 不过用`inline-quote`代替 `` ` ``
在`inline-quote`中, 你只能使用`,`而不能使用`,@`, 这是为了防止生成的compiler
macro意外的破坏函数语义.

用`define-inline`重新定义刚才提到的`my-aref`

``` lisp
(require 'inline)
(define-inline my-aref-inline (arr idx)
  (inline-quote (aref ,arr ,idx)))
```

``` lisp
(my-aref-inline [1 2 3] 0) ;=> 1
(macroexpand-all '(setf (my-aref-inline arr 1) 3)) ;=> (let* ((v arr)) (aset v 1 3))
```

用`macroexpand-all` 直接打开我们定义`my-aref-inline`的过程,得到

``` lisp
(progn
  (defun my-aref-inline
      (arr idx)
    (declare
     (compiler-macro my-aref-inline--inliner))
    (aref arr idx))
  :autoload-end
  (eval-and-compile
    (defun my-aref-inline--inliner
        (inline--form arr idx)
      (ignore inline--form)
      (catch 'inline--just-use
        (list 'aref arr idx)))))
```

可以看出来底层还是用的compiler macro的机制.

## inline-letevals

考虑函数

``` lisp
(defun pow2 (num)
  (* num num))
```

如何用`define-inline`定义其内联版本?

尝试直接用`inline-quote`

``` lisp
(define-inline pow2-inline (num)
  (inline-quote (* ,num ,num)))

(pow2-inline 3)                         ;=> 9
(pow2-inline 2)                         ;=> 4
(eval (byte-compile '(pow2-inline 2)))  ;=> 4
```

看起来没有问题, 继续测试

``` lisp
(defun my-side-effect-2 ()
  "Do a message, and return 2."
  (message "Side effect!")
  2)

(eval (byte-compile '(pow2-inline (my-side-effect-2)))) ;=> 4
```

这里, message "Side effect!"被发送了两次, 我们用`macroexpand`打开`pow2-inline`看
看

``` lisp
(*
 (my-side-effect-2)
 (my-side-effect-2))
```

看起来我们的参数`(my-side-effect-2)`被直接内联到了`*`的两个参数位置里, 造成
`my-side-effect-2` 被求值两次, 这显然不是我们想要的结果.

对于这种情况, `define-inline` 为我们提供了`inline-letevals` 来控制一个表达式只被
计算一次

``` lisp
(define-inline pow2-inline-2 (num)
  (inline-letevals (num)
    (inline-quote
     (* ,num ,num))))

(eval (byte-compile '(pow2-inline-2 (my-side-effect-2)))) ;=> 只有一次"Side Effect!"
```

展开`pow2-inline-2`的调用

``` lisp
(let ((print-gensym t)
      (print-circle t))
  (prin1-to-string (macroexpand-all '(pow2-inline-2 (my-side-effect-2)))))
;; => "(let* ((#1=#:num (my-side-effect-2))) (* #1# #1#))"

```

可以看出来`inline-letevals`类似与我们编写macro时用let和gensym保护expression的方
式.

# 使用function+compiler-macro与直接使用macro的区别

从语法上看macro不能作为高阶函数的参数使用(当然你可以拐着弯用lambda包裹macro). 而
function可以.

compiler macro和macro一样, 会被Emacs的Eager macroexpansion机制打开.

在老版本的Emacs中, 有人喜欢用宏替代内联函数, 这在新版本中的Emacs完全没有必要, 函
数和宏完全是两种不同语义的东西, 如果你需要内联函数优化, 请使用compiler macro, 或
者`define-inline`这种上层包装.

# 用compiler macro做内联和使用`defsubst`的内联有什么区别?

`defsubst`是另一种定义内联函数的方式, 对比compiler macro, `defsubst`内联的方式更
为保守.

`defsubst` 无法用`macroexpand`展开, 因此`defsubst`定义的inline function不能作为
`setf`的form.

比如, `defsubst`会保持lisp function对argument严格从左到右求值的逻辑. 同样也会建
立函数专有的变量作用域. 比如上文用来举例的`pow2-inline`, 用`defsubst`可以直接定
义为

``` lisp
(defsubst pow2-subst (num)
  (* num num))
  
(eval (byte-compile '(pow2-subst (my-side-effect-2)))) ;只有一次"Side Effect!"
```


使用compiler macro时, 求值策略是由用户自行决定的.

``` lisp
(defun say-is (somthing type)
  (message "%s is %s" something type))

(define-inline simple-case (something type)
  (inline-letevals (something)
    (inline-quote
     (cl-case ,something
       ((donkey (say-is ,something ,type)))
       ((rabbit (say-is ,something ,type)))))))
```

这里我们没有用`inline-letevals`保护`type`变量, 因为我们知道cl-case的两个分支不可
能同时执行, 而type被作为函数`say-is`的参数, `say-is`会将其eval. 这样我们就达成了
类似`lazy evaluation`的效果

# 相关讨论

[见emacs-china论坛](https://emacs-china.org/t/elisp-compiler-macro/10552)
