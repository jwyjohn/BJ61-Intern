open Type
open Base
module OptionTyped = struct
  type tm = (* named term *)
    | Var of string
    | Lam of string * ty option * tm 
    | App of tm * tm
    | Int of int
    | Bool of bool
    | Add of tm * tm
    | Sub of tm * tm
    | Lt of tm * tm        (* 比较整数 tm1 < tm2 *)
    | Eq of tm * tm        (* 判断 **整数** 是否相等 tm1 = tm2 *)
    | If of tm * tm * tm   (* if tm1 then tm2 else tm3 *)
    | Fix of string * ty option * tm (* fix f : T = tm *)
  
  (** 注入空类型标注, Untyped -> OptionTyped, 以供 pretty print *)
  let rec inject (tm:Term.Named.tm) : tm = 
    let module Untyped = Term.Named in
    match tm with
    | Untyped.Var(name) -> Var(name)
    | Untyped.Lam(bind, body) -> Lam(bind, None, inject body)
    | Untyped.App(tm1, tm2) -> App(inject tm1, inject tm2)
    | Untyped.Int(n) -> Int(n)
    | Untyped.Bool(b) -> Bool(b)
    | Untyped.Add(tm1, tm2) -> Add(inject tm1, inject tm2)
    | Untyped.Sub(tm1, tm2) -> Sub(inject tm1, inject tm2)
    | Untyped.Lt(tm1, tm2) -> Lt(inject tm1, inject tm2)
    | Untyped.Eq(tm1, tm2) -> Eq(inject tm1, inject tm2)
    | Untyped.If(tm1, tm2, tm3) -> If(inject tm1, inject tm2, inject tm3)
    | Untyped.Fix(bind, body) -> Fix(bind, None, inject body)

  (** 擦除类型标注, OptionTyped -> Untyped, 以供动态求值  *)
  let rec erase (tm:tm) : Term.Named.tm =
    let module Untyped = Term.Named in
    match tm with
    | Var(name) -> Untyped.Var(name) 
    | Lam(bind, _, body) -> Untyped.Lam(bind, erase body)
    | App(tm1, tm2) -> Untyped.App(erase tm1, erase tm2)
    | Int(n) -> Untyped.Int(n)
    | Bool(b) -> Untyped.Bool(b)
    | Add(tm1, tm2) -> Untyped.Add(erase tm1, erase tm2)
    | Sub(tm1, tm2) -> Untyped.Sub(erase tm1, erase tm2)
    | Lt(tm1, tm2) -> Untyped.Lt(erase tm1, erase tm2)
    | Eq(tm1, tm2) -> Untyped.Eq(erase tm1, erase tm2)
    | If(tm1, tm2, tm3) -> Untyped.If(erase tm1, erase tm2, erase tm3)
    | Fix(bind, _, body) -> Untyped.Fix(bind, erase body)
  
  exception ExpectType of tm

  (** 强制类型标注, OptionTyped -> Typed, 以供静态检查, 若无类型标注, 则抛出 ExpectType tm *)
  let rec force (tm:tm) : Type.Typed.tm = 
    let module Typed = Type.Typed in
    match tm with
    | Var(name) -> Typed.Var(name) 
    | Lam(bind, Some(ty), body) -> Typed.Lam(bind, ty, force body)
    | Lam(_, None, _) -> raise (ExpectType(tm))
    | App(tm1, tm2) -> Typed.App(force tm1, force tm2)
    | Int(n) -> Typed.Int(n)
    | Bool(b) -> Typed.Bool(b)
    | Add(tm1, tm2) -> Typed.Add(force tm1, force tm2)
    | Sub(tm1, tm2) -> Typed.Sub(force tm1, force tm2)
    | Lt(tm1, tm2) -> Typed.Lt(force tm1, force tm2)
    | Eq(tm1, tm2) -> Typed.Eq(force tm1, force tm2)
    | If(tm1, tm2, tm3) -> Typed.If(force tm1, force tm2, force tm3)
    | Fix(bind, Some(ty), body) -> Typed.Fix(bind, ty, force body)
    | Fix(_, None, _) -> raise (ExpectType(tm))

  (*  match force tm with
    | exception(ExpectType(tm)) -> ...
   *)

end

type tm = OptionTyped.tm
type cmd = 
  | Let   of string * tm
  | Eval  of string * int
  | Check of string * ty option
  

type status = | Error | Termination | Suspension
let eval (tm:tm) (times:int) : tm list * status * tm =
  let tm = tm |> OptionTyped.erase |> Term.drop_name in
  let rec aux tm seq times = begin
    if Dynamics.value tm then (seq, Termination, tm) 
    else if times = 0 then (seq, Suspension, tm)
    else match Dynamics.step tm with
    | (Dynamics.Error, tm) -> (seq, Error, tm)
    | (Dynamics.OK,    tm) -> aux tm (tm::seq) (times-1)
  end in 
  let (seq, status, tm) = aux tm (tm::[]) times in
  let tm = tm |> Term.give_name |> OptionTyped.inject in
  let seq = seq |> List.rev |> List.map 
  ~f:(fun tm->tm |> Term.give_name |> OptionTyped.inject)
  in (seq, status, tm)


let check (tm:tm) : ty option = tm 
  |> OptionTyped.force
  |> Statics.check (Map.empty (module String))


(* 存储 symbol -> term 的持久化数据结构 *)
type tbl = (string, tm, String.comparator_witness) Map.t

exception UnknownSymbol of string
let subst_tbl tbl (term:tm) = 
  let open OptionTyped in
  let rec aux dep map tm : tm = begin
    let reaux = aux dep map in
    match tm with
    | Var(name) -> 
      ( match Map.find map name with
      | Some(_) -> tm
      | None -> 
        ( match Map.find tbl name with
        | Some(tm) -> tm
        | None -> raise (UnknownSymbol(name))
        )
      )
    | Lam(bind, tyo, body) -> 
      let map = (Map.set map ~key:bind ~data:dep) in
      Lam(bind, tyo, aux (dep+1) map body)
    | App(tm1, tm2) -> 
      App(reaux tm1, reaux tm2)
    | Int(n) -> Int(n)
    | Bool(b) -> Bool(b)
    | Add(tm1, tm2) -> Add(reaux tm1, reaux tm2)
    | Sub(tm1, tm2) -> Sub(reaux tm1, reaux tm2)
    | Lt(tm1, tm2) -> Lt(reaux tm1, reaux tm2)
    | Eq(tm1, tm2) -> Eq(reaux tm1, reaux tm2)
    | If(tm1, tm2, tm3) -> If(reaux tm1, reaux tm2, reaux tm3)
    | Fix(bind, tyo, body) -> 
      let map = (Map.set map ~key:bind ~data:dep) in
      Fix(bind, tyo, aux dep map body)
  end in aux 0 (Map.empty (module String)) term



type response = 
  | Define of string * tm
  (* NotFound(symbol, tm) : tm 中自由变量 symbol 未在 tbl 中找到 *)
  | NotFound of string * tm 
  | Evaluation of tm list * status * tm 
  | IllTyped of string * tm
  | WellTyped of string * tm * ty
  (* IllCheck(symbol, tm, ty_true, ty_false) ty_true 是实际类型, *)
  | IllCheck of string * tm * ty * ty
  (* tm 缺乏应有的类型标注 *)
  | ExpectType of tm

let exec tbl cmd : response * tbl = 
  match cmd with
  | Let(name, tm) -> (
    match subst_tbl tbl tm with
    | exception(UnknownSymbol(symbol)) -> (NotFound(symbol, tm), tbl)
    | tm -> (Define(name, tm), Map.set tbl ~key:name ~data:tm )

  )
  | Eval(name, times) ->  
    let response = 
    ( match Map.find tbl name with
    | Some(tm) -> 
      let (seq, status, tm) = eval tm times in
      Evaluation(seq, status, tm)
    | None -> NotFound(name, OptionTyped.Var(name))
    ) in (response, tbl)
  | Check(name, tyo) ->
    ( match Map.find tbl name with
    | None -> (NotFound(name, OptionTyped.Var(name)), tbl)
    | Some(tm) -> 
      ( match check tm with
      | exception(OptionTyped.ExpectType(tm)) -> (ExpectType(tm), tbl)
      | tyo' ->
        let response = 
        ( match tyo, tyo' with
        | Some(ty), Some(ty') -> 
          if Type.(ty=ty') then  WellTyped(name, tm, ty)
          else IllCheck(name, tm, ty', ty)
        | None,     Some(ty ) -> WellTyped(name, tm, ty)
        |   _ ,     None      -> IllTyped(name, tm)
        )in (response, tbl)
      )
    )
