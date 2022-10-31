open Lab2
open Base
open Stdio

let file_name = "test.stlc" (* 测试指令所在的文件, 请按测试需求修改文件内容 *)

(* 
  lab-2 **不**要求你实现指令相关的部分, 这只是为了方便你进行测试.

  lab-2 实现的 Simply Typed Lambda Calculus(STLC) 作为一个编程语言,
  需要提供指令与程序员进行交互, 例如下方的 let 可以减少编写重复的代码. 
  STLC 麻雀虽小, 却五脏俱全, 相比 lambda calculus 更接近实际使用的编程语言了.

  下面是各个指令的详细介绍, 下列简单的例子给一个直观感受:
  ( 注: 你可以复制粘贴例子到测试文件中, 运行 ` dune test ` 以测试你的实现 )
  `
  let n = (\x. x) (3 + 4)
  eval n
  check n Int
  `
  注意: 在文本文件 `test.stlc` 中, 你不需要进行转义, 因此写 '\' 而非 "\\".
 *)
 
(* 指令 let
  指令 let symbol = term 

  let 指令能给 term 给予一个符号(symbol), 以减少多次书写相同的项.
  例如 lab-1 中我们曾定义的求和函数 sum, 在不使用 Y 组合子时可以如下定义:
  `
  let sum_aux = \ aux. \ n. 
    if n = 0 then n else n + (aux aux (n-1)) end
  let sum = sum_aux sum_aux
  `
  其中 sum_aux sum_aux 中的 sum_aux 指的是 \ sum. \ n.  ... 的部分

  这里的 let 指令只是提供了方式去重复使用 sum_aux, 
  但是 let 不能出现自指, 即下列 let 指令会抛出错误:
  ` let sum = \ n. if n = 0 then n else n + (sum (n-1)) end `
  因为在定义 `if n = 0 then n else n + (sum (n-1)) end` 这一 term 时
  `sum` 仍然是一个自由变量, 在 lab-2 中, 我们要求 term 是 closed.
 *)

(* 指令 eval symbol [n] 会调用你编写的 step 函数

  eval 指令能够对某个符号(symbol)对应的 term 进行小步求值, 
  eval symbol [n] 表示对于 symbol 对应的 term 进行至多 n 次小步动态语义转换,
  其中 n 可选, 默认值为 1000.
  如果 n 步以内出错, 则返回转换序列与错误来源; 
  如果 n 步以内终止, 则返回转换序列与最终值;
  如果 n 步尚未终止, 则返回转换序列与 n 步的结果.
 *)
(* 指令 check symbol [type] 会调用你编写的 check 函数

  check 指令能够对某个符号(symbol)对应的 term 进行类型检查
  check symbol [type] 表示对于 symbol 对应的 term 进行类型检查,
  其中 type 可选, 若不为空, 则检查给定的 term 之类型 与此 type 是否相等.
 *)

let main () = begin
  let channel = In_channel.create file_name in
  let lexbuf = Lexing.from_channel channel in
  let cmds = Parser.cmds Lexer.token lexbuf in
  let tbl = Map.empty (module String) in
  let open Cmd in
  List.fold cmds ~init:tbl ~f:(fun tbl cmd ->
    
    let (response, tbl) = exec tbl cmd in
    let open Caml.Format in
    let open Syntax.Stringify in 
    let fmt = std_formatter in
    ( match response with
    | Define(symbol, tm) -> fprintf fmt "定义: %s = @[%a@]@ " symbol pp_tm tm
    | NotFound(name, tm) -> fprintf fmt "错误: @[%a@] 中自由变量 %s 未在环境中找到@ " pp_tm tm name
    | Evaluation(seq, status, tm) -> (
      match status with
      | Error -> fprintf fmt "错误: 出现运行时错误, 错误来源 @[%a@],"
      | Termination -> fprintf fmt "成功: 求值结果为 [@%a@],"
      | Suspension -> fprintf fmt "悬挂: 求值未在指定步数内终止, 中途结果为 [@%a@],"
    ) pp_tm tm; 
      fprintf fmt "小步转换序列为:@[";
      List.iter seq ~f:(fun tm -> fprintf fmt "@[%a@];@ " pp_tm tm);
      fprintf fmt "@]";
    | IllTyped(symbol, tm) -> 
      fprintf fmt "错误: %s = @[%a@] 不良赋型@ " symbol pp_tm tm
    | WellTyped(symbol, tm, ty) -> 
      fprintf fmt "成功: %s = @[%a@] 赋型为@[%a@]@ " symbol pp_tm tm pp_ty ty
    | IllCheck(symbol, tm, ty_true, ty_false) ->
      fprintf fmt "错误: %s = @[%a@] 类型应为@[%a@], 不为@[%a@]" 
      symbol pp_tm tm pp_ty ty_true pp_ty ty_false
    | ExpectType(tm) -> 
      fprintf fmt "错误: @[%a@] 缺乏类型标注" pp_tm tm
    ) ;
    tbl
  )
end

let _ = main()