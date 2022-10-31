# PPL Lab2
Note : Currently, only simplified Chinese version(of course, with some academic words in English) is available.

## 实验要求
请在实验前阅读本文件, 以免错过重要信息.
在本实验中, 你将使用 OCaml 实现一个 Simply Type Lambda Calculus, STLC 以及递归函数拓展 `fix`. 
原则上, 代码文件中的注释足以帮助你完成实验, 如有问题, 请联系我们.
亦可参考[补充材料](https://github.com/ZJU-PPL/supplements)与教材 PFPL 的第 4~8 章与第 19 章.

## 项目结构
`lib/` 下的文件:
- `info.ml`
    你的个人信息 **应该** 填写在此处.
- `term.ml` `dynamics.ml` 
    词项 term 的抽象语法树结构, 基于无名表示的 subst 以及小步求值动态语义.
- `type.ml` `statics.ml` 
    类型 type 的抽象语法树结构以及静态语义(类型检查).
- `todo.ml`
    所有的 Todo 异常 以及 fix 拓展部分的实验指导.
- `test.ml` `test.stlc` 
    可用于自我测试, 请阅读 `test.ml` 内的注释, 按需修改`test.stlc`, 并运行 `$ dune test`.
- `fresh.ml` `lexer.mll` `parser.mly` `syntax.ml` `cmd.ml` 
    这是助教为你提供的帮助函数库, 你**不需要**关心这一部分的代码实现.
    若有需要, 你只需阅读函数名, 并借助 `$ dune utop` 来查看对应函数的类型.

## 实验组成
实验分为三部分, 未完成前一部分, 亦可进行后一部分, 评测系统将分别测试:
1. STLC 动态语义 `term.ml` `dynamics.ml`
2. STLC 静态语义 `type.ml` `statics.ml`
3. PCF STLC 的 fix 拓展 `todo.ml`

仅就实验指导而言, 你应当按如下顺序阅读文件中的注释:
1. `info.ml` 填写个人信息
2. `term.ml` 了解 term 的语法树结构
3. `dynamics.ml` 实现动态语义, 消灭 `Todo.Dynamics`
4. `type.ml` 了解 type 的语法树结构以及带有类型标注的 term 的语法树结构
5. `statics.ml` 实现静态语义, 消灭 `Todo.Statics`
6. `todo.ml` 实现 fix 拓展, 消灭 `Todo.Fixpoint`

## 分数组成

### 动态语义
|测试点|分数|备注|
|:-:|:-:|:-:|
|tm_src_ast|2|表面语法转抽象语法|
|tm_ast_src|2|抽象语法转表面语法|
|tm_seq|6|ast小步语义转换序列|
|tm_seq_fail|4|ast小步语义出错|
|step|28|ast小步语义实现|
|total|42|动态语义|


### 静态语义
|测试点|分数|备注|
|:-:|:-:|:-:|
|ty_ast_src|2|表面语法转抽象语法|
|ty_result|6|手动检查词项类型|
|check|36|类型检查|
|total|44|静态语义|

### fix 拓展
|测试点|分数|备注|
|:-:|:-:|:-:|
|fib_aux|2|通过 Z combinator 实现的|
|fib_fix|2|通过 `fix` 实现的|
|fix_ext|10|实现 `fix`|
|total|14|fix 拓展|

