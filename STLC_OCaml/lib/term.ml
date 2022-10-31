(* 阅读本文件之前, 请确保你在 `info.ml` 中填写了学号信息. *)

open Base

(* 取消三种 warning, 你也可以按需增减.
    27 是 "unused variable"
    32 是 "unused value declaration"
    39 是 "unused rec flag"
*)
[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 

module Named = struct
  (*
    类似于 lab-1, 我们在此定义了抽象语法树, 添加了若干运算符(operator)
    这些运算符主要是为了添加两种基本数据类型: 整数和布尔值
    以及如何使用这些数据类型进行运算, 加法(+)、减法(-)、条件判断(if)等.
   *)
  type tm = (* tm 是 term 的简写 *)
    | Var of string
    | Lam of string * tm
    | App of tm * tm

    | Int of int
    | Add of tm * tm
    | Sub of tm * tm
    | Lt of tm * tm        (* 比较整数 tm1 < tm2 *)
    | Eq of tm * tm        (* 判断 **整数** 是否相等 tm1 = tm2 *)

    | Bool of bool
    | If of tm * tm * tm   (* if tm1 then tm2 else tm3 *)

    | Fix of string * tm   (* fix f = tm *)
end
(*
  为使后续实验进行顺利, 你有必要对上述抽象语法树足够熟悉.
  并且, 由于相较 lab-1 中的 lambda calculus, lab-2 添加了许多分支,
  这使得书写与阅读抽象语法树不甚方便, 为此我们提供下列帮助函数:
 *)

(*
  1. 提供了编译器前端, 将字符串转换为抽象语法树, 例如:
  "\\ f. f (3 + 2)" 表示的是 Lam("f", App(Var("f"), Add(Int(3), Int(2))))
  在 utop 环境里, 尝试 ` OptionTyped.erase @@ Parse.tm "\\ f. f (3 + 2)" `
  便可得到对应的 Named.tm (目前无需理解 OptionTyped.erase 的作用)
  你也可以尝试下列帮助函数
  ` Parse.tm_nd "\\ f. f (3 + 2)" ` 其中 nd 是 n AME d     的简写
  ` Parse.tm_nl "\\ f. f (3 + 2)" ` 其中 nl 是 n AME l ESS 的简写
  
  ( 注: 编译器前端的实现, 你将会在下学期编译原理的课程上学到, 这超出本课范围. 不过无需担心,
    热心的助教为你实现了 `lexer.mll` 与 `parser.mly`, 而且你不必了解细节, 可以忽略. 
    出于简单的考量, 实验中不会出现负整数, 请你也尽量不要写负整数, 否则会导致错误 )
*)
let example_src_1 : string = "\\ f. f (3 + 2)"
let example_ast_1 : Named.tm = Named.(
  Lam("f", App(Var("f"), Add(Int(3), Int(2)))))

(*
  2. 提供了pretty printer, 将抽象语法树转换为字符串, 例如布尔非函数:
  Lam("bool", If(Var("bool"), Bool(false), Bool(true))) 对应
  "\\ bool. if bool then false else true end"

  在 utop 环境里, 尝试 ` 
    Stringify.tm @@ OptionTyped.inject Named.(
      Lam("bool", If(Var("bool"), Bool(false), Bool(true))))
  ` 便可得到对应的字符串 (同样地, 目前无需理解 OptionTyped.inject 的作用)
  你也可以尝试下列帮助函数:
  ` Stringify.tm_nd Named.(
    Lam("bool", If(Var("bool"), Bool(false), Bool(true))))
  ` 
  而 `Stringify.tm_nl` 是 n AME l ESS 版本的.

  ( 注: pretty printer 的实现位于 `syntax.ml`, 你也可以忽略此文件. )
*)
let example_ast_2 : Named.tm = Named.(
  Lam("bool", If(Var("bool"), Bool(false), Bool(true))))
let example_src_2 : string = "\\ bool . if bool then false else true end"

(* 
  不知你注意到没有, "\\ bool. if bool then false else true end" 中使用了双斜线 "\\"
  这是因为在字符串中, '\'用来转义, "\\" 才表示字符'\' 本身.
  我们可以使用 {| |} 要求不进行转义, 即写成如下形式也是对的:
  {| \ bool. if bool then false else true end |}
  你可以在 utop 里尝试:
  `
  Parse.tm_nd {| \ bool. if bool then false else true end |}
  `
 *)
 
(* 为使你尽快熟练使用这两组帮助函数, 请完成下列任务: *)

(* 表达式, 请保证绑定变量的命名一致 (2 分)
  根据下列抽象语法树, 写出对应的字符串:
  App(Lam("x", If( Lt(Var("x"), Int(0)), Sub(Int(0), Var("x")), Var("x")))
  , If(Bool(true), Bool(false), Bool(true)))
  *)
let tm_ast_src : string = {| (\ x . if x < 0 then 0 - x else x end) if true then false else true end |}

(* Z 组合子, 请保证绑定变量的命名一致 (2 分)
  根据下列字符串, 写出对应的抽象语法树:
  {| \ f. (\ x. f(\ y. x x y)) (\ x. f(\ y. x x y)) |}
 *)
let tm_src_ast : Named.tm = Named.(Lam ("f"
                                   , App(Lam ("x", App(Var "f", Lam("y", App(App(Var "x", Var "x"), Var "y"))))
                                   , Lam("x", App(Var "f", Lam ("y", App(App (Var "x", Var "x"), Var "y")))))))

(*
  至此, 你应该对 Named.tm 这一类型充分熟悉, 至于 Named 这一称呼,
  是因为本文件剩余的代码还要处理 Named.tm <-> Nameless.tm 的转换
  以及基于 nameless representation 的 substitution.
  也就是 lab-1 中你曾经实现过的内容, 这里直接给出一份实现.
  若你不甚明白如何实现两种表示的相互转换, 可以阅读以下实现作为参考;
  若你已经明白, 下面的实现可以简略看一眼函数名称, 
    只有当你做 fix 拓展部分时才需要返回到这里.
  接下来你应前往 `dynamics.ml` 去解决动态语义问题.
 *)

module Nameless = struct
  type var = 
    | Free of string
    | Bound of int
  type tm = 
    | Var of var
    | Lam of tm
    | App of tm * tm
    | Int  of int
    | Bool of bool
    | Add of tm * tm
    | Sub of tm * tm
    | Lt of tm * tm      (* 比较整数 tm1 < tm2 *)
    | Eq of tm * tm      (* 判断 **整数** 是否相等 tm1 = tm2 *)
    | If of tm * tm * tm (* if tm1 then tm2 else tm3    *)
    | Fix of string * tm (* fix bind = tm *)
  (* 
    仔细的你或许发现了, 即便是 Nameless 的 tm, Fix 也有 string 而不是 var,
    下面的 nameless representation 中, fix bind = body
    body 中的 bind 是 Var(Free bind), 而不是 Var(Bound n)
    这也是为什么你需要实现 subst_free 与 subst_bound 的 | Fix（bind, body) 部分 
   *)

  (* subst_free b x a = [b/x]a *)
  let subst_free (b:tm) (x:string) (a:tm) : tm =
    let rec aux (b:tm) (x:string) (a:tm) = begin
      let reaux = aux b x in 
      match a with 
      | Var(Free(name)) -> if String.(x=name) then b else a
      | Var(_) | Int(_) | Bool(_) -> a
      | Lam(tm) -> Lam(reaux tm)
      | App(tm1, tm2) -> App(reaux tm1, reaux tm2)
      | Add(tm1, tm2) -> Add(reaux tm1, reaux tm2)
      | Sub(tm1, tm2) -> Sub(reaux tm1, reaux tm2)
      | Eq(tm1, tm2) -> Eq(reaux tm1, reaux tm2)
      | Lt(tm1, tm2) -> Lt(reaux tm1, reaux tm2)
      | If(tm1, tm2, tm3) -> If(reaux tm1, reaux tm2, reaux tm3)
      | Fix(bind, body) -> Fix(bind, reaux body)
    end in aux b x a

  (* subst_bound b a = [b/0]a *)
  let subst_bound (b:tm) (a:tm) : tm =
    let rec aux (b:tm) (i:int) (a:tm) = begin
      let reaux = aux b i in
      match a with
      | Var(Bound(index)) -> if i=index then b else a
      | Var(_) | Int(_) | Bool(_) -> a
      | Lam(tm) -> Lam(aux b (i+1) tm)
      | App(tm1, tm2) -> App(reaux tm1, reaux tm2)
      | Add(tm1, tm2) -> Add(reaux tm1, reaux tm2)
      | Sub(tm1, tm2) -> Sub(reaux tm1, reaux tm2)
      | Eq(tm1, tm2) -> Eq(reaux tm1, reaux tm2)
      | Lt(tm1, tm2) -> Lt(reaux tm1, reaux tm2)
      | If(tm1, tm2, tm3) -> If(reaux tm1, reaux tm2, reaux tm3)
      | Fix(bind, body) -> Fix(bind, reaux body)
    end in aux b 0 a 
end

let drop_name (tm : Named.tm) : Nameless.tm = 
  let rec aux dep map tm : Nameless.tm = begin
    let reaux = aux dep map in
    match tm with
    | Named.Var(name) -> 
      ( match Map.find map name with
      | None -> Nameless.(Var(Free(name)))
      | Some(level) -> Nameless.(Var(Bound(dep-level-1)))
      )
    | Named.Lam(bind, body) -> 
      let map = (Map.set map ~key:bind ~data:dep) in
      Nameless.Lam(aux (dep+1) map body)
    | Named.App(tm1, tm2) -> 
      Nameless.App(reaux tm1, reaux tm2)
    | Named.Int(n) -> Nameless.Int(n)
    | Named.Bool(b) -> Nameless.Bool(b)
    | Named.Add(tm1, tm2) -> 
      Nameless.Add(reaux tm1, reaux tm2)
    | Named.Sub(tm1, tm2) -> 
      Nameless.Sub(reaux tm1, reaux tm2)
    | Named.Lt(tm1, tm2) -> 
      Nameless.Lt(reaux tm1, reaux tm2)
    | Named.Eq(tm1, tm2) -> 
      Nameless.Eq(reaux tm1, reaux tm2)
    | Named.If(tm1, tm2, tm3) ->
      Nameless.If(reaux tm1, reaux tm2, reaux tm3)
    | Named.Fix(bind, body) -> Nameless.Fix(bind, reaux body)
  end in aux 0 (Map.empty (module String)) tm


exception UnknownIndex of int
let give_name (tm : Nameless.tm) : Named.tm = 
  let gen = Fresh.init ~base:"x" ~forbid:[] in
  let rec aux gen dep map tm = begin
    let reaux = aux gen dep map in
    match tm with
    | Nameless.Var(Free(name)) -> Named.Var(name)
    | Nameless.Var(Bound(index)) -> 
    (   match Map.find map (dep-index-1) with
      | Some(name) -> Named.Var(name)
      | None -> raise (UnknownIndex(index))
    )
    | Nameless.Lam(body) -> 
      let (name, gen) = Fresh.get gen in
      let map = Map.set map ~key:dep ~data:name in
      Named.Lam(name, aux gen (dep+1) map body)
    | Nameless.App(tm1, tm2) ->
      Named.App(reaux tm1, reaux tm2)
    | Nameless.Int(n) -> Named.Int(n)
    | Nameless.Bool(b) -> Named.Bool(b)
    | Nameless.Add(tm1, tm2) ->
      Named.Add(reaux tm1, reaux tm2)
    | Nameless.Sub(tm1, tm2) ->
      Named.Sub(reaux tm1, reaux tm2)
    | Nameless.Lt(tm1, tm2) ->
      Named.Lt(reaux tm1, reaux tm2)
    | Nameless.Eq(tm1, tm2) ->
      Named.Eq(reaux tm1, reaux tm2)
    | Nameless.If(tm1, tm2, tm3) ->
      Named.If(reaux tm1, reaux tm2, reaux tm3)
    | Nameless.Fix(bind, body) -> Named.Fix(bind,reaux body)
  end in aux gen 0 (Map.empty (module Int)) tm


(*
  给仍然读到最后的耐心的读者 : 至此, 你便解锁了 let 指令.

  关于如何使用指令, 请看 `test.ml`. lab-2 **不**要求你实现指令相关的部分.
  lab-2 实现的 Simply Typed Lambda Calculus(STLC) 作为一个编程语言,
  需要提供指令与程序员进行交互, 例如这里的 let 可以减少编写重复的代码. 
  STLC 麻雀虽小, 却五脏俱全, 相比 lambda calculus 更接近实际使用的编程语言了.
 *)