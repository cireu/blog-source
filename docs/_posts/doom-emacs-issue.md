---
type: post
date: 2019-10-18
tag:
  - emacs
---

# doom-emacs 配置的问题

[doom-emacs](https://github.com/hlissner/doom-emacs) 最近似乎挺流行了, 许多
Spacemacs 用户宣称自己要切换到 doom-emacs 上来, 也有不少人给 Emacs 新人推荐 doom-emacs
作为 starter kit. 然而经过我一段时间的研究, 我发现 doom-emacs 存在一些的问题使其对
试图通过学习它来进阶 Emacs Lisp 用户, 以及需要深度 hack in Emacs 的用户来说并不
是那么友好.

虽然本文试图揭示 doom-emacs 配置内部存在的一些问题, 但我必须声明我并没有长时间使用
doom-emacs 的经验, 入门 Emacs 的时候, 我用过一阵子
[Spacemacs](https://github.com/syl20bnr/spacemacs). 此后我就一直使用我[自己写的
配置](https://framagit.org/citreu/emacs.d/blob/master/etc/init-emacs-lisp.el#L3)
本文基于我的 Elisp 插件编写经验, 学习配置的时候对 doom-emacs 底层机制的了解而得出, 如果有
什么谬误, 欢迎来信探讨.

## 宏

### 被滥用的宏

doom-emacs 作者似乎很喜欢炫技, 在配置里引入了各种各样的 macro. doom-emacs 有一堆感
叹号结尾的宏, `after!`, `lambda!`[^lambda], `lambda!!`, `delq!`. 写起配置来仿佛
是在写一篇措辞激烈的徼文一样!

过度使用宏, 会带来理解上的困难. 而且 doom-emacs 里的这些宏, 真的有包装成宏的必要么?
比如`lambda!`

``` lisp
(defmacro λ! (&rest body)
  "Expands to (lambda () (interactive) ,@body)."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))
```

完全可以直接写成简单清楚的`(lambda () (interactive) ..forms)`比起`lambda!`的确多
打了不少字. 但能更的提示用户这是一个 interactive lambda.

如果这种 macro 的确能减少配置中的 boilerplate, 那就算了. 然而当你用`grep`搜索
doom-emacs 源码时, 实际上`lambda!` 只被使用了 4 次[^lambda2], 这令我感到十分无语,
因为你完全可以用`defun`直接定义一个有名字的 interactive 函数, 而不需要用`fset`+
`lambda!`这种令人费解的操作...

### 拙劣的宏设计

混乱的交互界面, 严重缺乏正交性和统一性的设计让 doom-emacs 对用户极其不友好, 往往只
有对 doom-emacs 理解很深的用户, 才能驾驭代码中各种各样的 Avada Kedavra. 这里举几个
我遇到的简单例子

#### 缺乏正交性

doom-emacs 除了用`use-package`提供的`:config`关键字延迟加载配置, 又重新发明了一个
`after!`宏. 很多时候`:config`和`after!` 往往混在一起, 让你摸不着头脑.

#### 宏的 "重载"

`add-hook!`宏上支持各种各样的 "重载"[^add-hook], 对比两段等价代码的不同写法

``` lisp
(add-hook! 'emacs-lisp-mode-hook 'ignore)
```

``` lisp
(add-hook! emacs-lisp-mode 'ignore)
```

 这里头的奥秘在于`emacs-lisp-mode` 没有 quote 的时候`add-hook!`宏会自动给你加上一
 个`-hook`的后缀. 看起来是可以节省打字功夫的精巧设计, 然而可能作者自己也记不住这
 些规则, doom-emacs 里有时用起`'xxx-hook`, 有时直接用`xxx`. 不如直接统一用
 `'xxx-hook`, 和 Emacs 的`add-hook`保持一致.

#### 粗暴的宏实现

前面说过, 只有对 doom-emacs 十分理解的深度"黑客", 才能准确的把握这些`XXX!`的脾气.
然而, 当你深入每个 macro 的内部试图理解它们的工作机制时, 却发现这些宏的实现往往是
天坑.

作者知道宏是一种很好的抽象手段, 但是在宏内部却缺乏适当的抽象, 完全可以使用"模式
匹配" 来简化的解析手段, 却要用上各种各样的`car`, `caar`, `caddddr`... 等自带混淆
的原始的列表操作, 看得你眼花缭乱, 不知所云.

## Layer 的错误设计

不止在 doom-emacs 里, 许多通用配置都提供了类似 "Layer" 这样的概念[^layer], 用来划分对不同
功能的支持. 比如在 Spacemacs 里, C++ 语言和 Java 语言分别在一个不同的 layer, 这样
你就可以按自己的需要分别使用不同的 Layer, 比如我最近要写一点 Lisp, 我就打开 Lisp
layer, 几天后我对 OCaml 感兴趣, 我就又打开 OCaml layer, 等等...

不得不说结构化的配置是有好处的. 这样不同语言的配置可以交给不同的, 精于本语言的
Emacs 用户来参与维护. 然而经过仔细分析后, 我发现这些重量级配置的 layer system, 很
大程度上没有做对.

Layer 这一概念有点类似于 Emacs 里的 "Minor Mode". 按 GNU Emacs User Manual 的[说
法](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html)

> A “minor mode” provides optional features that users may enable or disable
> independently of the choice of major mode. Minor modes can be enabled
> individually or in combination.

事实上, minor mode 比 layer 更强大灵活, 因为 minor-mode 允许你执行 "切换" 这个操作,而
这些重量级配置重新发明的 layer 系统只能通过修改配置和重启来切换 layer. 比如将我的
[Emacs Lisp Mode 配
置](https://framagit.org/citreu/emacs.d/blob/master/etc/init-emacs-lisp.el#L3)转
化为一个 minor mode :P

``` lisp
(defun cm/on-elisp-mode-load ()
  (setq-local mode-name "Elisp")
  (setq-local outline-regexp ";;;;* [^ \t\n]"))

(define-minor-mode cm/elisp-config-mode
  "Enable my Elisp configuration."
  (if cm/elisp-config-mode
      (progn
        (add-hook 'emacs-lisp-mode-hook
                  #'cm/on-elisp-mode-load)
        (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
        (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer))
    (progn
      (remove-hook 'emacs-lisp-mode-hook
                   #'cm/on-elisp-mode-load)
      (define-key emacs-lisp-mode-map (kbd "C-c C-c") nil)
      (define-key emacs-lisp-mode-map (kbd "C-c C-b") nil))))
```

当我想启用这个设置的时候, 我只需要在 Emacs 内使用<kbd>M-x
cm/elisp-config-mode</kbd>, 来应用我的配置, 并且我可以随时关闭这个 minor mode, 免
得它影响我接下来的工作. 这种简单而直接的机制, 配合上 macro 来减少部分 boilderplate 代码,
达成的效果不差于单独设计的 layer 系统.[^doom-recompile] 

另外一点值得商榷的是, 所有 layer 的配置文件都集中在这些重量级配置的仓库里, 这意
味着你每次更新都必须拉取所有 layer 的更新, 即使你从来不会去用某些 layer!

## 配置 VS 包

其实上述提到的问题, 根本的原因就在于这些重量级配置的维护者们没有认识到配置与包的
区别. 不同于市面上大多数编辑器使用的 TOML, YAML, JSON...基于声明式语言的配置文件,
Emacs 的配置是用一门通用语言[^gpl] Emacs Lisp 写成的. 从本质上来看, 包和配置似乎
并没有什么区别, 他们都是一些写了 Emacs Lisp 的文本文件, Emacs 会自动读取并解释这
些 Elisp 文件－－ Emacs 本体就是一个 Elisp 的解释器!

某种程度上, 我们所写的 Emacs 配置文件, 本质上就是一个 Elisp package, 不过这种
"package"里, 有每个人自己基于自己实际情况的小修小补. 然而当你试图把配置通用化,
重新引入像 package 一样的规范那就完全有必要了.

另一方面, 将不同的配置分离到不同的 package 里, 还可以充分利用包管理器自动生成
autoload 文件的机制实现增量式的配置文件载入[^pack-autoload], 优化 Emacs 的启动速
度.

这些重量级配置的作者, 一开始并没有认识到把这些通用配置当作 package 来严格执行规
范化的重要性. 随着需求不断变大, 各种各样的 Ad-hoc patch 像俄罗斯套娃一样一层盖一
层, 他们发现土胚起的高楼终究是不稳定的, 但代码库已经积重难反...[^commit]

## 结语

我不是要劝你退坑 doom-emacs 转投 Spacemacs, 或者其他的配置了. 这些问题不但
doom-emacs 有, Spacemacs 里也有, 在任何复杂一点的配置文件(甚至可能包括我自己的),都
可能存在. 我也不是要吹鼓 "通用配置已死" 的原教旨主义者. 比起旗帜鲜明的站立场, 我
更希望从我的这些反思和研究中, 得出一种现代化的大型配置组织指导方案.

#### 适度抽象

为了达到通用性, 一定程度的抽象是有必要的.
[use-package](https://github.com/jwiegley/use-package) 提供了按每个 package 分隔配
置的抽象, 我们也许需要更上层的抽象(比如按不同编程语言抽象). 有了上层抽象, 我们就
可以抛弃一些不必要的底层抽象(如`add-hook!` `after!`), 统一使用上层抽象的接口来控
制. 上层抽象必须具有良好的扩展性[^use-pkg-like]以应付变化的配置需求. 

#### 分离复杂的 Hack 成 package

当你使用了复杂的 Hack 来完成一些特定的功能, 为什么不把他们提取出来成 package, 发布
到 MELPA 呢. 不要害羞的认为自己的 hack 是没有价值的, 重复造轮子的工作. 对于个人开发
者, 发布 package 不但可以造福社区里的其他人, ELPA 仓库的审核员也会指导你的 Elisp 技巧,
让你更快的进步. 对于通用型配置来说, 上文中我已经提到了模块化和分离配置的好处, 在
此就不再赘述.

doom-emacs 和 Spacemacs 某种程度上做到了一定的分离. 然而正如上述, 这种分
离并不彻底. 将配置框架抽象出来, 不同的 layer 封装成不同的 package, 或许是更好的解决
方案.

#### 必须要有自己的 ELPA 仓库[^melpa-builder]
  
有的配置过于零碎, 打包成 package 时不一定能被 MELPA 接受, 这时你可以自建一个 ELPA 服务
器托管. 另一方面, 由于 MELPA 滚动更新的性质, 与 doom-emacs 和 Spacemacs 这些提供一个稳
定版配置的努力(master branch)相悖[^stable], 经常会造成旧配置的 Hack 代码跟不上新插
件的情况. 搭建自己的 ELPA 镜像维持插件稳定性就十分有必要了.

## 延伸阅读


  * [什么是"脚本语言"](http://www.yinwang.org/blog-cn/2013/03/29/scripting-language)[^sh]

<!-- footnotes -->

[^commit]: 截止到发稿时间, Spacemacs 的 develop 分支已经有 10250 个提交,
    doom-emacs 的 develop 分支已经有 10543 个提交, 他们的 master 分支都已经处于放
    弃维护的状态

[^doom-recompile]: doom-emacs 按其标准操作安装额外的 package 的时需要重新执行 make 然
    后重新启动 Emacs

[^gpl]: 虽然听起来有点怪异, 但是 Elisp 并不含为特定领域设计的程序结构(除了 macro),
    所以可以视为一门通用编程语言

[^pack-autoload]:
    <https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging-Basics.html#Packaging-Basics>

[^lambda]: 最搞笑的是这个 lambda!宏还用的是 Unicode 字符λ, 这大概会让不少终端 Emacs
    用户抓狂.

[^lambda2]: <https://github.com/hlissner/doom-emacs/blob/develop/core/autoload/debug.el#L255>

[^add-hook]:
    <https://github.com/hlissner/doom-emacs/blob/develop/core/core-lib.el#L226>

[^melpa-builder]: 事实上搭建一个自己的 ELPA 服务器并不困难, 你可以复用 melpa 的[构建系统](https://github.com/melpa/package-build) 

[^stable]: <https://emacs-china.org/t/topic/4167>

[^sh]: 我的一些观点于王垠先生的在该文章中提到的观点类似

[^use-pkg-like]: 这可以参考 use-package 的自定义 handler.

[^layer]: 最开始的 layer 概念大概要追溯到 purcell 开始用 `init-xxx.el` 来分离不同功
    能的配置的时候了.
