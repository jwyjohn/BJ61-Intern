
(* 阅读本文件之前, 请确保你阅读过 `term.ml` `dynamics.ml`. *)

open Base

type ty = (* ty 是 type 的简写 *)
  | Bool
  | Int
  | Lam of ty * ty (* 函数类型 ty1->ty2 *)
  

(* 类型的相等判断 
   在其他文件中使用时, 可以写成 ` Type.(ty1=ty2) `
   其中 Type 指的是本文件 `type.ml` .
 *)
let rec (=) ty1 ty2 : bool = 
  match ty1, ty2 with
  | Bool, Bool | Int, Int -> true
  | Lam(ty1A, ty1B), Lam(ty2A, ty2B) -> 
    (ty1A = ty2A) && (ty1B = ty2B)
  | _, _ -> false
  

(* 
  与 `term.ml` 那边类似, 模块 Syntax.Parse 与 Syntax.Stringify 提供了帮助函数,
  请你在 utop 里尝试下列两个 **不同** 的例子, 了解 ty1->ty2 是右结合的.
  1. 字符串 -> 抽象语法树
    ` Parse.ty "(Bool->Int)->Int" `
  
  2. 抽象语法树 -> 字符串
    ` Stringify.ty Type.(Lam(Bool, (Lam(Int, Int)))) `
  
 *)
(*
  根据下列字符串, 写出对应的抽象语法树(2 分):
  "Int->(Bool->Int)->(Bool->Bool)"
 *)
let ty_ast_src : ty = Lam (Int, Lam(Lam(Bool, Int), Lam(Bool, Bool)))

(*
  还记得在 `term.ml` 里出现过的 OptionTyped.erase 和 OptionTyped.inject 吗?
  这是因为, 类型检查需要添加信息, 而我们实现的编译器前端与pretty printer 兼容两种语法,
  即有类型标注与无类型标注的两种语法. 下面 Typed 模块定义了有类型标注的 term.

  lab-2 要求进行类型检查时, 函数的参数必须有类型标注, 即形如:
  "\\ x : Int. x + 3"
  为得到 Typed.tm 的值, 请在 utop 中尝试 : 
  ` OptionTyped.force @@ Parse.tm "\\ x: Int. x + 3" `
  其中 OptionTyped.force 检查是否有类型标注, 并强制类型标注, 否则抛出异常.

  而先前见到的两个 OptionTyped 的帮助函数, 分别是
  擦除(erase)类型标注 OptionTyped.erase  : OptionTyped.tm -> Named.tm 
  注入(inject)空标注  OptionTyped.inject : Named.tm -> OptionTyped.tm
  
  称为 OptionTyped, 是因为类型标注部分是 ty option, 试对比:
  module OptionTyped = struct
    type tm = (* named term *)
      | Var of string
      | Lam of string * ty option * tm 
      (* 详见 `cmd.ml` *)
  end
  与下列的定义
 *)
module Typed = struct
  
  type tm = (* named term *)
    | Var of string (* 注:  这里定义的 tm 是 named, 
                            因为类型检查时使用有名表示更方便一些 *)
    | Lam of string * ty * tm 
    | App of tm * tm
    | Int of int
    | Add of tm * tm
    | Sub of tm * tm
    | Lt of tm * tm        (* 比较整数 tm1 < tm2 *)
    | Eq of tm * tm        (* 判断 **整数** 是否相等 tm1 = tm2 *)
    | Bool of bool
    | If of tm * tm * tm   (* if tm1 then tm2 else tm3 *)
    | Fix of string * ty * tm (* fix f : T = tm *)
end

(* 
  ctx 是静态语义检查时的环境(context), 一般记作 Γ, 表示变量名 x 到其类型 T 的映射.
  关于 Base.Map, 你应当在 lab-1 中了解了足够的用法, 详细的文档见:
  https://ocaml.org/p/base/v0.15.0/doc/Base/Map/index.html 
 *)
type ctx =
  (string, ty, String.comparator_witness) Map.t

(* 接下来, 请你前往 `statics.ml` 解决静态语义问题. *)