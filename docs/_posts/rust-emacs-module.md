---
type: post
date: 2020-04-05
tag:
  - emacs
  - rust
---

# 用Rust横向扩展Emacs功能

## 摘要

在Emacs 25中，Emacs引入了『动态模块』（dynamic module）的机制，允许开发者使用C
语言规定的动态链接库格式扩展Emacs本身所缺乏的能力。

本文阐释了使用Rust开发动态模块相对于C语言的优势，并介绍讲解如何使用
[ubolonto](https://github.com/ubolonton/emacs-module-rs) 开发的
[emacs-module-rs](https://github.com/ubolonton/emacs-module-rs) 简化Rust动态模组开发
流程。

## 为什么选择Rust而不是C ##

C语言作为一门上世纪70年代发明的编程语言，其历史局限性十分严重。可以认为，C语言的
一些缺陷，阻碍了动态模块这一扩展方式的流行。

* 语言本身过于底层，缺乏表达能力，需要浪费精力在内存管理上。对聚焦于代码逻辑本身
  的快速开发不够方便。
* 缺乏良好的类型约束，容易写出内存不安全的代码。
* 缺少中心化的包分发管理系统，获取构建依赖十分麻烦。
* 不同用户上的C语言编译器和C语言运行时不同，在用户构建使用时容易出现难以复现的
  BUG

Rust作为一门新时代的，聚焦于内存安全，底层效率以及并发的语言，正好完美解决了C语
言带来的这些痛点。

* Rust本身支持最常用的三大操作系统，代码和构建系统无须做过多调整即可跨平台使用。
* Rust与C之间有良好的互操作性，即使需要调用C语言轮子也不会过于繁琐。
* Rust使得开发者在编写动态模组时无需像C语言一样为了处理内存细节而烦恼。可
  以集中精神在代码逻辑上。
* Emacs module的Rust绑定具有良好的抽象封装，避免了直接与`emacs_module.h`里定义的
  底层函数交互的繁琐细节。
* Rust具有优秀的社区生态，可以复用大量开源社区的轮子，节约了开发时间。

## 目标读者群 ##

本文假定你有一定的 Rust 和 Elisp 编程经验。如果你没有，可以先从这里开始

- [Rust by Example（中文）](https://github.com/rust-lang-cn/rust-by-example-cn)
- [An Introduction to Programming in Emacs Lisp（英
  文）](https://www.gnu.org/software/emacs/manual/eintr.html)

## 环境 ##

本节记录写下本文所用的操作系统以及相关软件的版本

* `Archlinux`
* `Emacs 26.3`
* `rustc 1.41.1 (f3e1a954d 2020-02-24)`
* `clang 9.0.1`
* `llvm 9.0.1`

## 初试牛刀

### 创建项目 ###

执行下列命令创建一个空白Rust项目

``` sh
cargo init --lib greeting
cd greeting
```

修改 `Cargo.toml`文件

``` toml
[package]
name = "greeting"
version = "0.1.0"
edition = "2018"
# 我们通常希望把 Emacs 相关的 package 发布在 ELPA 而不是 crates.io 上
publish = false

[lib]
# 重要，否则 Rust 不会生成兼容 C ABI 的动态库
crate-type = ["cdylib"]

[dependencies]
# 重要，创建 Emacs 动态模块的关键依赖
emacs = "0.13"
```

### 编码 ###

`src/lib.rs`

``` rust
use emacs::{defun, Env, Result, Value};

// 定义一个 C int 类型的名为 plugin_is_GPL_compatible 的全局变量，否则 Emacs 会拒绝加
// 载模块 :)
emacs::plugin_is_GPL_compatible!();

// 定义模块入口，大部份情况我们不需要它做任何事。
#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

/// 输出一条你好信息
#[defun]
fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
    env.message(&format!("Hello, {}!", name))
}
```

### 编译 ###

``` sh
cargo build

# 对于名为 libxxx 的动态库文件，须更名为 crate 的命名，以保证 emacs-module-rs 生
# 成的 provide form 提供的 feature 与文件名相同

# Linux 
ln -sv target/debug/libgreeting.so greeting.so

# Mac
ln -sv target/debug/libgreeting.dylib greeting.so

# Windows
# Windows10 默认禁止一般用户创建软链接，你可以参照
# https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/create-symbolic-links 
# 为你自己启用软链接创造权限，这里使用硬链接作为替代。
mklink target/debug/greeting.dll greeting.dll
```

### 加载运行 ###

``` lisp
(add-to-list 'load-path "<path/to/your/project/>")
(require 'greeting)
(greeting-say-hello "Emacs")
```

若一切顺利，minibuffer会弹出信息 `Hello, Emacs!`

## `emacs::Value` ##

`emacs::Value` 是在 Rust 表示 Elisp 值的代理类型，每个 `Value` 代表一个对 Elisp
值的引用。由于具体的内存分配实际上是Elisp VM的GC在管理，`Value` 在Rust里的拷贝和
移动是十分轻量的（因此实现了 `Copy` trait）。

Rust无法直接利用一个 `Value`，只有 `emacs-module.h` 提供了转换函数的的基本类型可
以用于转换。在底层函数之上，`emacs-module-rs` 又提供了 `IntoLisp` 和`FromLisp`
trait用于抽象Rust类型到 `Value` 之间的转换。

下列类型实现了这些traits。你也可以为自定义类型实现这些traits来实现复杂数据的转换。

* `Value` 对应自己 :P
* `i64` 对应一个 Elisp fixnum（64位有符号整型
* `f64` 对应一个 Elisp float（64位双精度浮点）
* `String` 对应一个 Elisp字符串（不含text property）
* `emacs::Vector` 对应一个Elisp向量
* `()` 对应 Elisp 的 `nil`
* `Option<T>` 对应一个可为 `nil` 的 Elisp 值，具体类型由 `T` 决定


## `#[defun]` ##

`emacs::defun` 是 `emacs-module-rs` 最主要的魔法，这个属性宏修饰一个Rust的函数
定义，使其转变为可以在Elisp VM里调用的模块函数。

被修饰的函数可以接收任意数量的所有权类型，且类型实现了`FromLisp` trait的参数，最终返回
`emacs::Result<T>`其中 `T` 实现`IntoLisp` trait。简化了返回和取用Elisp值时手动转型的
模板代码。

``` rust
/// 一个愚蠢但是简单的例子，这个注释会被导入 Emacs 的帮助系统中，
/// 通过 M-x describe-function 查阅。
#[defun]
fn add(lhs: i64, rhs: i64) -> emacs::Result<i64> {
    Ok(lhs + rhs)
}
```

### 命名修饰 ###

Elisp 缺乏命名空间系统，每个包都拥有自己的命名前缀来防止产生命名冲突。
`emacs-module-rs` 导出的 Elisp 函数的命名修饰格式为 `<包名>[相对于crate的Rust模块路
径名]<函数名>`

Rust的 snake_case 风格命名会被转换成 kebab-case 的 Lisp 风格命名，表示路径的
`::` 也会被转换为 Lisp 风格的 `-`

## 调用Elisp函数 ##

### 调用全局静态函数 ###

`emacs::Env` 类型上静态挂载了常用的Elisp函数，这些函数通常都是 `emacs-module.h`
直接暴露出来的。

``` rust
env.intern("defun")?;

env.message("Hello")?;

env.type_of(5.into_lisp(env)?)?;

env.provide("my-module")?;

env.list((1, "str", true))?;
```

要捕捉一个 `Env`来调用这些函数，请在 `defun` 修饰的函数定义里额外加入一个类型为
`&Env` 的参数。

``` rust
/// 是的，这函数将一个字符串 intern 为 Elisp symbol。但是你为什么不直接在 Elisp 里
/// 用 intern 呢
#[defun]
fn silly_intern<'e>(env: &'e Env, s: String) -> emacs::Result<Value<'e>> {
    env.intern(s.as_ref())
}
```

### 调用任意函数 ###

`emacs::Env::call(func, args)` 可以调用任意Elisp函数，其中

* `func`可为标识函数名的字符串（调用对应的 `symbol-function`），或者可调用的
  `Value` （如闭包）
* `args`可为一个 `&[Value]` 类型，或者一个包含了1-12（受限于实现，暂时上限12个）
  个元素，且每个元素都实现了 `IntoLisp` trait的元组

``` rust
// (list "str" 2)
env.call("list", ("str", 2))?;


let list = env.intern("list")?;
// (symbol-function 'list)
let subr = env.call("symbol-function", [list])?;
// (funcall 'list "str" 2)
env.call(list, ("str", 2))?;
// (funcall (symbol-function 'list) "str" 2)
env.call(subr, ("str", 2))?;
subr.call(("str", 2))?; // 简写

// (add-hook 'text-mode-hook 'variable-pitch-mode)
env.call("add-hook", [
    env.intern("text-mode-hook")?,
    env.intern("variable-pitch-mode")?,
])?;
```

``` rust
/// 将一个 Elisp Vector 转换为 Elisp List
#[defun]
fn listify_vec(vector: Vector) -> Result<Value> {
    let mut args = vec![];
    for i in 0..vector.size()? {
        args.push(vector.get(i)?)
    }
    vector.0.env.call("list", &args)
}

```

<!-- ::: warning -->
<!-- 牢记 **单一职责原则**（KISS），尽量不要在Rust模块函数大量调用Elisp函数， -->
<!-- Rust模块函数应当只完成其必要的计算工作。如果你需要在数据计算前后进行必要的校验或 -->
<!-- 处理，你可以用Elisp额外写一个函数执行这些逻辑，并在内部调用一个模块函数。 -->
<!-- ::: -->

## 向Elisp VM嵌入Rust复合数据类型 ##

将一个Rust的复合数据类型完整转换为Elisp里的数据类型往往很困难。另一方面，在Rust
线程和Elisp VM间反复进行数据转换会降低模块执行的效率。因此，Elisp提供`user-ptr`
类型用于承载对Elisp不透明的外部类型实例。

::: tip
为了使导出的自定义类型有意义，你还需要额外编写操作该数据类型的模块函数。
:::

`user-ptr` 类型的释放由Elisp VM的GC 托管，比起一般Rust复合数据类型有额外约束：

* `user-ptr` 类型实例必须位于堆上
* `user-ptr` 类型具体生命周期编译时不可推断，被移动进Elisp VM的复合数据类型
  必须满足 `'static` 生命周期约束
* 由于无法重新获得所有权，Rust只能用不可变引用访问 `user-ptr` 类型，常常需要使用
  内部可变性（单线程 `RefCell`，多线程锁机制，原子类型等）
  
在非多Rust线程的情况下，`emacs-module-rs` 的 `#[defun]` 宏可以简化大部份样板代
码。

``` rust
use std::collections::HashMap;
use emacs::{defun, Env, Result, Value};

#[emacs::module(name = "rs-hash-map", separator = "/")]
fn init(env: &Env) -> Result<()> {
    type Map = HashMap<String, String>;

    // 标记函数返回一个用 RefCell 包裹的 user-ptr 类型
    // #[defun] 宏会自动将其包裹成 Box<RefCell<Map>>
    #[defun(user_ptr)]
    fn make() -> Result<Map> {
        Ok(Map::new())
    }

    // 在 #[defun] 的参数中，引用类型总表示由 #[defun(user-ptr)] 嵌入的 user-ptr
    // 的实例的引用
    #[defun]
    fn get(map: &Map, key: String) -> Result<Option<&String>> {
        Ok(map.get(&key))
    }

    #[defun]
    fn set(map: &mut Map, key: String, value: String) -> Result<Option<String>> {
        Ok(map.insert(key,value))
    }

    Ok(())
}
```

``` lisp
(let ((m (rs-hash-map/make)))
  (rs-hash-map/get m "a")     ; -> nil

  (rs-hash-map/set m "a" "1") ; -> nil
  (rs-hash-map/get m "a")     ; -> "1"

  (rs-hash-map/set m "a" "2") ; -> "1"
  (rs-hash-map/get m "a"))    ; -> "2"
```

## 错误处理

### 在Rust中处理Elisp错误 ###

::: warning
**强烈不建议在Rust代码中处理Elisp代码的错误！** 在Rust代码中处理可能
的Elisp的错误繁琐且容易造成内存不安全（涉及unsafe代码）。请直接使用 `?` 操作符向
上抛出在Rust代码中调用Elisp代码时可能的抛错。这些错误最终会以`rust-error` 的错误
类型暴露给Elisp VM。
:::

### 在Elisp代码中处理Rust错误 ###

`emacs-module-rs` 暴露了两种错误类型供用户在Elisp端用`condition-case`处理。

* `rust-error` 所有由Rust制造的错误。
* `rust-wrong-type-user-ptr` 当尝试把一个lisp值转换为一个user-ptr在Rust里的值时，
  若类型不匹配，则抛出该错误。该错误是 `rust-error` 的子类型错误。

``` rust
// 如果 value 内含有的不是 RefCell<HashMap<String, String>>，或者甚至不是一个
// user-ptr，Elisp 端会抛出一个 rust-wrong-type-user-ptr 错误。
let r: &RefCell<HashMap<String, String>> = value.into_rust()?;
```

::: tip
如果你需要在Rust代码里抛出类型更精确的Elisp错误，可以尝试在Rust里调用Elisp函数
`signal` 。

``` rust
#[emacs::module(name = "test")]
fn init(_: &Env) -> Result<()> {
    #[defun]
    fn signal_wrong_type(e: &Env) -> Result<()> {
        let s = "Mismatched type!".to_owned();
        e.call("signal", (e.intern("wrong-type-argument")?, e.list((s,))?))?;
        unreachable!();
    }
    Ok(())
}
```

``` lisp
(condition-case e
    (test-signal-wrong-type)
  (wrong-type-argument
   (message (error-message-string e)))) ;; => "Wrong type argument: \"Mismatched type!\""
```
:::

### 在Elisp中处理Rust panic ###

::: warning
跨越FFI边界的unwind是一个 **未定义行为**。`emacs-module-rs` 会尝试用
`catch_unwind` 捕捉 panic，遗憾的是这并不总奏效－－有的panic根本不使用unwind实现！
`catch_unwind` 可以捕捉到的panic会变成一个 Elisp 错误，而无法捕捉的错误其行为则
依赖于具体实现（如调用abort实现panic会把你的Emacs连带崩掉 :P）

panic常代表不可恢复的错误，表明程序的逻辑出了问题。尽管有办法在 Elisp 中捕捉一个
Rust panic，但我不会在这里教你怎么做－－比起掩盖错误的发生，更应该找出错误的原
因。
:::

### Rust错误之间的互相处理 ###

`emacs-module-rs`本身使用
[failure](https://github.com/rust-lang-nursery/failure)库处理错误。如果你恰好
也使用了 failure 处理错误，可以直接使用 `?` 操作符抛出你的错误，
`emacs-module-rs`会将其统一处理为 `rust-error` 类型的Elisp错误抛出给Elisp VM。

其他类型的错误，在转换为 `failure::Error` 后也可以按同样的方式抛出给Elisp VM。

## 延伸阅读 ##

本文对编程动态模块中不常见的细节做了部分省略以保证作为入门读物的可读性，要完整的
理解`emacs-module-rs` ，请移步[emacs-module-rs's user guide（英
文）](https://ubolonton.github.io/emacs-module-rs/latest/)

要更深入理解Rust语言以及 `emacs-module-rs` 背后的魔法，阅读Rust官网提供的教程是
不错的选择。

* [The Rust Programming Language（英文）](https://doc.rust-lang.org/book/)
* [Rust Nomicon（英文）](https://doc.rust-lang.org/nomicon)

下列是一些用 Rust 构建动态模块的 Emacs 插件列表，也许可以在你写新模块的时候
帮助到你。

* [fuz.el](https://github.com/rustify-emacs/fuz.el)
* [emacs-tree-sitter](https://github.com/ubolonton/emacs-tree-sitter)
