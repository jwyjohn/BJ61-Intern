module Parse = struct
let cmds (source:string) : Cmd.cmd list =
  let buf = Lexing.from_string source in
  Parser.cmds Lexer.token buf
  

let tm (source:string) : Cmd.OptionTyped.tm = 
  match cmds ("let x = " ^ source) with
  | Cmd.Let(_, tm)::[] -> tm
  | _ -> failwith "Impossible"
  

let ty (source:string) : Type.ty =
  match cmds ("check x " ^ source) with
  | Cmd.Check(_, Some(ty))::[] -> ty
  | _ -> failwith "Impossible"
  

let tm_nd (source:string) : Term.Named.tm =
  source |> tm |> Cmd.OptionTyped.erase
  
let tm_nl (source:string) : Term.Nameless.tm = 
  source |> tm_nd |> Term.drop_name
  
  
end

module Stringify =  struct

(** pretty_print to string *)
let pp_to_str pp (ast:'a) : string = 
  let open Format in
  ignore @@ flush_str_formatter ();
  fprintf str_formatter "%a@ " pp ast;
  flush_str_formatter ()
  

let rec pp_ty fmt (ty:Type.ty) = 
  let open Format in
  let open Type in
  match ty with
  | Bool -> fprintf fmt "Bool"
  | Int  -> fprintf fmt "Int"
  | Lam((Lam(_, _) as ty1), ty2) -> fprintf fmt "(%a)->%a" pp_ty ty1 pp_ty ty2
  | Lam(ty1, ty2) -> fprintf fmt "%a->%a" pp_ty ty1 pp_ty ty2
  

let rec pp_tm fmt (tm:Cmd.OptionTyped.tm) =
  let open Format in
  let open Cmd.OptionTyped in
  let aux_add_sub (sym:string) tm1 tm2 = begin
    ( match tm1 with
    | Lt(_, _) | Eq(_, _)  | Lam(_, _, _) | If(_, _, _) | Fix(_, _, _)
      -> fprintf fmt "(%a)" pp_tm tm1
    | Add(_, _) | Sub(_, _) 
    | App(_, _)
    | Var(_) | Int(_) | Bool(_)
      -> fprintf fmt "%a" pp_tm tm1
    ); 
      fprintf fmt "@ %s@ " sym;
    ( match tm2 with
    | Lt(_, _) | Eq(_, _) | Lam(_, _, _) | If(_, _, _) | Fix(_, _, _)
    | Add(_, _) | Sub(_, _) 
      -> fprintf fmt "(%a)" pp_tm tm2
    | App(_, _) 
    | Var(_) | Int(_) | Bool(_) 
      -> fprintf fmt "%a" pp_tm tm2
    )
  end in
  let aux_lt_eq (sym:string) tm1 tm2 = begin
    ( match tm1 with
    | Lam(_, _, _) | If(_, _, _) | Fix(_, _, _)
      -> fprintf fmt "(%a)" pp_tm tm1
    | Eq(_, _) | Lt(_, _) 
    | App(_, _) 
    | Var(_) | Int(_) | Bool(_) | Add(_, _) | Sub(_, _) 
      -> fprintf fmt "%a" pp_tm tm1
    ); 
      fprintf fmt "@ %s@ " sym;
    ( match tm2 with
    | Lam(_, _, _) | If(_, _, _) | Fix(_, _, _)
    | Eq(_, _) | Lt(_, _) 
      -> fprintf fmt "(%a)" pp_tm tm2
    | App(_, _) 
    | Var(_) | Int(_) | Bool(_) | Add(_, _) | Sub(_, _) 
      -> fprintf fmt "%a" pp_tm tm2
    )
  end in
  let aux_app tm1 tm2 = begin
    ( match tm1 with
    | Lam(_, _, _) 
    | Add(_, _) | Sub(_, _)
    | Lt(_, _) | Eq(_, _)
        ->  fprintf fmt "(%a)@ " pp_tm tm1
    | _ -> fprintf fmt "%a@ " pp_tm tm1
    );
    ( match tm2 with
    | App(_, _) | Lam(_, _, _)
    | Add(_, _) | Sub(_, _)
    | Lt(_, _)  | Eq(_, _)
        -> fprintf fmt "(%a)" pp_tm tm2
    | _ -> fprintf fmt "%a" pp_tm tm2
    )
  end in
  match tm with
  | Var(name) -> fprintf fmt "%s" name
  | Lam(bind, Some(ty), body) -> fprintf fmt "\\ %s : %a . %a" bind pp_ty ty pp_tm body
  | Lam(bind, None, body) -> fprintf fmt "\\ %s . %a" bind pp_tm body
  | App(tm1, tm2) -> aux_app tm1 tm2
  | Int(n) -> fprintf fmt "%d" n
  | Bool(true) -> fprintf fmt "true"
  | Bool(false) -> fprintf fmt "false"
  | Add(tm1, tm2) -> aux_add_sub "+" tm1 tm2
  | Sub(tm1, tm2) -> aux_add_sub "-" tm1 tm2
  | Lt(tm1, tm2) -> aux_lt_eq "<" tm1 tm2
  | Eq(tm1, tm2) -> aux_lt_eq "=" tm1 tm2
  | If(tm1, tm2, tm3) -> fprintf fmt "if@ %a@ then@ %a@ else@ %a@ end" pp_tm tm1 pp_tm tm2 pp_tm tm3
  | Fix(bind, Some(ty), body) -> fprintf fmt "fix %s : %a = %a" bind pp_ty ty pp_tm body
  | Fix(bind, None, body) -> fprintf fmt "fix %s = %a" bind pp_tm body


let ty = pp_to_str pp_ty
let tm = pp_to_str pp_tm

let tm_nd (tm':Term.Named.tm) = tm' |> Cmd.OptionTyped.inject |> tm (* tm_named *)
let tm_nl (tm:Term.Nameless.tm) = tm |> Term.give_name |> tm_nd  (* tm_nameless *)

end