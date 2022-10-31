exception Dynamics 
exception Statics  
(*
  请确保你阅读过 dynamics 与 statics 部分才往下阅读.
 *)

exception Fixpoint

(* 拓展 : fix 
  温馨提示: 像先前的内容一样, 先是简单的练习, 然后才是实现语义.
 *)

(*  
  注意, STLC 动态语义是完全擦除类型后进行的(回忆 OptionTyped.erase ),
  这意味着, 我们可以像 lab-1 那样写出 Y 组合子以及递归函数.
  然而, 有一些事情发生了变化, lab-2 的动态语义是 applicative order(或说 eager),
  而不是 normal order (即某种 lazy) (参见 `dynamics.ml` 中函数 step 的 App 分支).
  因此我们不能使用 Y 组合子, 而只能改用类似的 Z 组合子.
  你应该在 `term.ml` 的练习中接触过 Z 组合子, 现在不妨进行对比:
 *)
let y_src : string = {| \ f. (\ x. f(      x x  )) (\ x. f(      x x  )) |}
let z_src : string = {| \ f. (\ x. f(\ y.  x x y)) (\ x. f(\ y.  x x y)) |}
(* 
  还记得课上曾讲过的例子 {| (\u.\v. v)((\x. x x)(\x. x x)) |} 吗?
  applicative order 会使得这一例子发散, Y 和 Z 的差别也就是在这里.
 *)

(*
  在有了 Z combinator 之后, 下面给出 sum 函数, 即 sum n = 0 + 1 + 2 + ... + n
  我们只给出辅助函数 sum_aux, 从 lab-1 中你应该知道 sum = z sum_aux. 
  现假设你已经看过 `test.ml` 中的内容, 你可以用 ` dune test `, 试一试运行 sum .
 *)
let sum_aux : string = {| \ aux. \ n.  if n = 0 then n else n + (aux (n-1)) end |}
(* 基于 Z 组合子的 fibonacci (2 分)
  现在请你写出递归计算 fibonacci 数列的函数, 并约定 fib 0 = fib 1 = 1.
  你仍然只需要给出辅助函数 fib_aux, 你可以使用 ` dune test ` 进行测试.
 *)

let fib_aux : string = {| \ aux. \ n.  if n < 2 then 1 else (aux (n-2)) + (aux (n-1)) end |}

(*
  不过, Z 组合子在 STLC 里是不可类型化的, 即 Z 组合子没有一个合适的类型.
  ( 注: 你可以尝试为 Z 组合子添加类型标注, 看看哪里出了问题 )
  这是因为递归函数有一个本质的问题: 递归函数可能不终止.
  在 lab-1 中, 不少同学因为死循环的问题而没有得到评测结果, 也是如此.

  业已证明, STLC 中良赋型的 (well-typed) 项不仅不会出现错误, 
  而且对其的小步求值转换序列总是能在有限步内终止.
  关于程序的可终止性, 或者说停机问题, 你或许会在计算理论课程中学习到,
  这与 lab-2 进行关系不大, 感兴趣者可自行查阅教材 PFPL 第 9 章 系统T.

  为了能够对递归函数做类型检查, 我们势必失去一些什么: 
  系统 T 选择了保证程序可终止性, 失去了编写递归函数的全能性;
  lab-2 选择了失去程序可终止性, 保留了编写递归函数的全能性.

 *)

(* lab-2 的 fix 拓展
  下面将对 fix 拓展进行简要阐述, 阅读后你就能完成 lab-2:终 ,
  另外也可以参考教材 PFPL 第 19 章 递归函数的系统 PCF.

  表面语法(surface  syntax):
    fix bind : type = body
  抽象语法(abstract syntax):
    |  Fix of string * type * tm 
    (* Fix(bind, type, body) *)

  下面以 sum 为例, 请理解 fix 的表面语法与抽象语法, 并完成习题 fib_fix.
 *)
let sum_fix : string = 
  {|  fix sum : Int->Int = \ n : Int .
      if n = 0 then 0 else n + (sum (n-1)) end
  |}
(* 基于 fix 拓展的 fibonacci (2 分)
  请同样写出 fibonacci 函数的 fix 版本, 并约定 fib 0 = fib 1 = 1.
 *)
let fib_fix : string = 
  {|  fix fib : Int->Int = \ n : Int .
      if n < 2 then 1 else (fib (n-2)) + (fib (n-1)) end
  |}
(*
  下面是 Fix 的语义部分, 你需要根据这些规则消灭 Todo.Fixpoint (10 分)
  你可以通过注释掉本文件中的 ` exception Fixpoint `,
  从 dune 的构建错误信息中找到所有 Todo.Fixpoint 的位置, 
  也可以在编辑器里搜索 `Todo.Fixpoint`, 他们分别在:
  - `term.ml`
  - `dynamics.ml`
  - `statics.ml`

  动态语义规则:
    --------------------------------------------------- Fix
    fix bind = body --> [fix bind = body/bind] body
  静态语义规则:
            Γ, bind : ty ⊢ body  : ty
    --------------------------------------------------- Ty-Fix
      Γ ⊢ (fix bind : ty = body) : ty

  至此, lab-2 的全部内容已经向你呈现, 请尽量完成.
 *)