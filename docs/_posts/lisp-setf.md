---
type: post
date: 2019-09-17
tag:
 - lisp
---
# setf 中 「f」 的本意

## (setf (car cons) 'cdr)

大多数人第一眼见到`setf`，可能会简单的把f理解成form。这种解释很符合直觉，因为我
们已经有了对symbol进行赋值操作的`setq`。而`setf`可以对form（广义变量）进行赋值。 

然而这个直觉是错误的。 Deutsch在他的论文《A Lisp machine with very compact
programs》中（[原文](http://www.softwarepreservation.org/projects/LISP/interlisp-d/Deutsch-3IJCAI.pdf)）提到 

> A more useful function is (SETFQ (fn arg1 ... argn) newvalue)
> which quotes the function name and evaluates everything else. 
> This allows RPLACA, for example, to be defined as (LAMBDA (X Y) (SETFQ (CAR X) Y) ).

这里面提到的`rplaca`在Elisp里叫做`setcar`

所以`setf`是`setfq`的缩写，f的意思是function。 另外在这篇论文中我们也可以看到MicroLisp引入了现在我们使用的广义变量的概念

## 参考

[原博客](https://g000001.cddddr.org/3715857294)
