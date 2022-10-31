{
open Parser
open Base

let keyword_table = Hashtbl.of_alist_exn (module String)
  [ ("let"  , TOK_LET  )
  ; ("eval" , TOK_EVAL )
  ; ("check", TOK_CHECK)
  ; ("if"   , TOK_IF   )
  ; ("then" , TOK_THEN )
  ; ("else" , TOK_ELSE )
  ; ("end"  , TOK_END  )
  ; ("true" , TOK_BOOL_LIT(true)  )
  ; ("false", TOK_BOOL_LIT(false) )
  ; ("fix"  , TOK_FIX  )
  ; ("Int"  , TOK_INT  )
  ; ("Bool" , TOK_BOOL )
  ]
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident_char = digit | lower | upper | '_'
let blank      = [' ' '\t' '\r']
let newline    = ['\n']

rule token = parse
  | eof { TOK_EOF }
  | blank+ { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | ':' { TOK_COLON }
  | '\\' { TOK_LAMBDA }
  | '.' { TOK_DOT }
  | '+' { TOK_ADD }
  | '-' { TOK_SUB }
  | '=' { TOK_EQ }
  | '<' { TOK_LT }
  | "->" { TOK_ARROW}
  | '(' { TOK_L_PAREN }
  | ')' { TOK_R_PAREN }
  | (lower|upper)(ident_char*) {
      let ident = Lexing.lexeme lexbuf in
      match Hashtbl.find keyword_table ident with
      | Some(token) -> token
      | None -> TOK_IDENT(ident)
   }
  | (digit+)  { TOK_INT_LIT (Int.of_string @@ Lexing.lexeme lexbuf )}
