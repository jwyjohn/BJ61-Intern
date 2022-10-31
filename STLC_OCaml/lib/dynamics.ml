
(* 阅读本文件之前, 请确保你阅读过 `term.ml`. *)

(* 我们接下来使用 nameless 进行动态语义的实现
   Term.Nameless 指的是 `term.ml` 这一模块中的 Nameless 模块
   如果你需要 Nameless <-> Named 之间的转换, 请使用
   Nameless 模块的 drop_name 与 give_name 这两个函数, 详见`term.ml`.
 *)

open Term.Nameless

[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 

(*
  下面定义的函数去判断哪些 term 是值, 
  即当 val t = true 时, t 是值.
  另一方面, 我们可以用规则去定义 judgement t val
  掌握规则与函数之间的转换关系, 对完成本实验有较大裨益, 
  这需要你去观察比较模仿练习.

  ----------- Val-Lam
  λx.t val

  ----------- Val-Int
  Int(n) val

  ----------- Val-Bool
  Bool(b) val

 *)
(* 使用 value 作为名字, 是因为 val 是 OCaml 保留的关键字. *)
let value : tm -> bool = fun (t:tm) -> match t with
  | Lam(_) | Int(_) | Bool(_) -> true
  | _ -> false


(* 
  在进行小步语义之前, 我们不妨来看几个简单的例子, 
  为方便起见, 下列都使用字符串而不是抽象语法树表示.
      4 - 2 + (5 + 2) 
  -->   2   + (5 + 2)
  -->   2   +    7
  -->       9
  上述例子说明了表达式 {| 4 - 2 + (5 + 2) |} 的求值过程
  并且上述转换序列是小步的, 没有进行跳过任何一个步骤. 
  下面使用 string list 表示了这一转换过程经过的步骤.
 *)
let example_src_seq : string list =
  [ {| 4 - 2 + (5 + 2) |}
  ; {|   2   + (5 + 2) |}
  ; {|   2   +    7    |}
  ; {|       9         |}
  ]
(*
  接下来, 请你模仿练习, 补充完整下列两个转换序列(各 3 分):
  关于求值过程的优先顺序, 你可以使用直觉上的那一个,
  即便你未能通过此处两个练习的测试, 也请继续往下完成小步求值语义.
 *)
let src_seq_1 : string list =
  [ {| if 2 + 3 < 5 then 0+1+2 else 8-2 end |}
  ; {| if 5 < 5 then 0+1+2 else 8-2 end |}
  ; {| if false then 0+1+2 else 8-2 end |}
  ; {| 8-2 |}
  ; {| 6 |}
  ]
let src_seq_2 : string list = 
  [ {| (\ f. f (5 - 3))(\ x. if x < 0 then 0 - x else x + 2 end) |}
  ; {| (\ x. if x < 0 then 0 - 2 else 2 + 2 end) 2 |}
  ; {| if 2 < 0 then 0 - x else 2 + 2 end |}
  ; {| if false then 0 - x else 2 + 2 end |}
  ; {| 2 + 2 |}
  ; {| 4 |}
  ]

(* 
  在实现小步求值语义之前, 我们还必须观察到某些表达式会出错.
  例如表达式 if 3 + 5 - 1 then f 3 else g 1 end 有下列转换序列:
 *)
let example_src_seq_fail : string list = 
  [ {| (if 3 + 5 - 1 then f 3 else g 1 end) + 6 |}
  ; {| (if   8   - 1 then f 3 else g 1 end) + 6 |}
  ; {| (if     7     then f 3 else g 1 end) + 6 |}
  ]
let example_src_seq_reason : string = 
  {| if     7     then f 3 else g 1 end |}
(*
  错误的原因是, 7 是一个 Int 而不是一个 Bool , 因此不能作为 if 的条件判断.
  {| if     7     then f 3 else g 1 end |} 无法继续小步转换.
  在 C 语言中, 7 会被隐式转换成 true, 但本实验设计的语言没有这样的设计, 
  这是因为隐式转换往往会带来不易察觉的动态语义问题.

  下面请你模仿上述例子, 补充完整转换序列, 并写出导致不能再进行小步转换的项(4 分):
 *)

let src_seq_fail : string list = 
  [ {| (\ f. f true)((\ x. if x then \y. y+1+1 else \y.y end)
    ((\ x. if x then false else true end)(3+1<2))) |}
  ; {| (\ f. f true)((\ x. if x then \y. y+1+1 else \y.y end)
  ((\ x. if x then false else true end)(4<2))) |}
  ; {| (\ f. f true)((\ x. if x then \y. y+1+1 else \y.y end)
  ((\ x. if x then false else true end) false)) |}
  ; {| (\ f. f true)((\ x. if x then \y. y+1+1 else \y.y end)
  (if false then false else true end)) |}
  ; {| (\ f. f true)((\ x. if x then \y. y+1+1 else \y.y end) true) |}
  ; {| (\ f. f true)(if true then \y. y+1+1 else \y.y end) |}
  ; {| (\ f. f true)(\y. y+1+1) |}
  ; {| (\y. y+1+1) true |}
  ; {| true+1+1  |}
  ]
let src_seq_reason : string = {| true+1+1 |}

(*
  接下来就是小步求值语义 t --> t', 这也是本文件最重要的内容(28 分).
  即实现函数 step t = t', 但注意到, step 函数的返回类型是 status * tm .
  我们已经说过小步求值可能出错, 例如 (true + 3) + 5 --> 错误 .
  我们使用 (Error, true+3) 表示错误及其来源, (OK, tm) 表示正确及其结果.
  
  温馨提示 : 你可能需要自己想一些测试例子检查自己的实现是否正确.
 *)
(* 
  消灭 Todo.Dynamics, 你便能解锁 eval 指令. 

  事实上, 为了方便你使用一些测试例子检查自己的实现是否正确, 
  你可以阅读 `test.ml` 中的指导, 通过 `dune test` 来做测试.
 *)
type status = | OK | Error 
let rec step (tm:tm) : (status * tm) = match tm with
  | Var(_) -> (Error, tm) (* Var 出错, 我们总假定 tm 是 closed term *)
  | Lam(_) | Int(_) | Bool(_) -> (OK, tm) (* value 不再发生变化 *)
  | App(t1, t2) -> (
    (* 
      下面是 App 的动态语义规则及其对应的实现,
      值得一提的是, 这种 App 的运算顺序与 lab-1 中的 normalization 不同
      如下运算顺序被称为 applicative order, 或说 eager evaluation
      而 lab-1 中的称为 normal order, 某种程度上是 lazy evaluation
          t1    --> t1'
      ------------------------ App-1
          t1 t2 --> t1' t2

      t1 val t2 -->    t2'
      ------------------------ App-2
          t1 t2 --> t1 t2'

      t1 val t2 val  t1=\x.t1'
      ------------------------ App-3
          t1 t2 --> [t2/x]t1'
     *)
    let is_val_1 = value t1 in
    let is_val_2 = value t2 in
    if is_val_1 && is_val_2 then (* App-3 *)
      match t1 with (* subst_bound b a = [b/0]a *)
      | Lam(t1') -> (OK, subst_bound t2 t1') 
      | _ -> (Error, tm)
    else if is_val_1 then (* App-2 *)
      match step t2 with
      | (Error, tm) -> (Error, tm) (* 小步求值出错, "短路"将错误传递出去 *)
      | (OK, t2') -> (OK, App(t1, t2'))
    else (* App-1 *)
      match step t1 with
      | (Error, tm) -> (Error, tm) (* 小步求值出错, "短路"将错误传递出去 *)
      | (OK, t1') -> (OK, App(t1', t2))
  )
  | Add(t1, t2) -> ( (* 这里使用括号, 是因为出现了内层 match 嵌套 *)
    (* 
      下面是 Add 的规则, 请你模仿 App 写出对应的实现:
            t1    --> t1'
      ---------------------- Add-1
            t1+t2 --> t1'+t2

      t1 val   t2 -->    t2'
      ---------------------- Add-2
            t1+t2 --> t1+t2'

      t1 val t2 val t1=Int(n1) t2=Int(n2)
      ------------------------------------ Add-3
            t1+t2 --> Int(n1+n2)
     *)
    let is_val_1 = value t1 in
    let is_val_2 = value t2 in
    if is_val_1 && is_val_2 then (* Add-3 *)
      match t1, t2 with
      | Int(n1), Int(n2) -> (OK, Int(n1+n2)) 
      | _ -> (Error, tm)
    else if is_val_1 then (* Add-2 *)
      match step t2 with
      | (Error, tm) -> (Error, tm) (* 小步求值出错, "短路"将错误传递出去 *)
      | (OK, t2') -> (OK, Add(t1, t2'))
    else (* Add-1 *)
      match step t1 with
      | (Error, tm) -> (Error, tm) (* 小步求值出错, "短路"将错误传递出去 *)
      | (OK, t1') -> (OK, Add(t1', t2))
  )
  | Sub(t1, t2) -> (
    (* 
      减法与加法有类似的规则, 还记得 作业-学点 OCaml 吗？
      请你模仿 Add 的规则写出 Sub 的规则, 并同样实现:

            t1    --> t1'
      ---------------------- Sub-1
            t1-t2 --> t1'-t2

      t1 val   t2 -->    t2'
      ---------------------- Sub-2
            t1-t2 --> t1-t2'

      t1 val t2 val t1=Int(n1) t2=Int(n2)
      ------------------------------------ Sub-3
            t1-t2 --> Int(n1-n2)
     *)
    let is_val_1 = value t1 in
    let is_val_2 = value t2 in
    if is_val_1 && is_val_2 then (* Sub-3 *)
      match t1, t2 with 
      | Int(n1), Int(n2) -> (OK, Int(n1-n2)) 
      | _ -> (Error, tm)
    else if is_val_1 then (* Sub-2 *)
      match step t2 with
      | (Error, tm) -> (Error, tm)
      | (OK, t2') -> (OK, Sub(t1, t2'))
    else (* Sub-1 *)
      match step t1 with
      | (Error, tm) -> (Error, tm)
      | (OK, t1') -> (OK, Sub(t1', t2))
  )
  (* Lt, Eq 的规则, 不能说相像, 只能说是一模一样. *)
    (* 
            t1    --> t1'
      ---------------------- Lt-1
            t1<t2 --> t1'<t2

      t1 val   t2 -->    t2'
      ---------------------- Lt-2
            t1<t2 --> t1<t2'

      t1 val t2 val t1=Int(n1) t2=Int(n2)
      ------------------------------------ Lt-3
            t1<t2 --> Bool(n1<n2)
     *)
  | Lt(t1, t2) -> (
    let is_val_1 = value t1 in
    let is_val_2 = value t2 in
    if is_val_1 && is_val_2 then (* Lt-3 *)
      match t1, t2 with 
      | Int(n1), Int(n2) -> (OK, Bool(n1<n2)) 
      | _ -> (Error, tm)
    else if is_val_1 then (* Lt-2 *)
      match step t2 with
      | (Error, tm) -> (Error, tm)
      | (OK, t2') -> (OK, Lt(t1, t2'))
    else (* Lt-1 *)
      match step t1 with
      | (Error, tm) -> (Error, tm)
      | (OK, t1') -> (OK, Lt(t1', t2))
  )
    (* 
            t1    --> t1'
      ---------------------- Eq-1
            t1=t2 --> t1'=t2

      t1 val   t2 -->    t2'
      ---------------------- Eq-2
            t1=t2 --> t1=t2'

      t1 val t2 val t1=Int(n1) t2=Int(n2)
      ------------------------------------ Eq-3
            t1=t2 --> Bool(n1=n2)
     *)
  | Eq(t1, t2) -> (
    let is_val_1 = value t1 in
    let is_val_2 = value t2 in
    if is_val_1 && is_val_2 then (* Eq-3 *)
      match t1, t2 with 
      | Int(n1), Int(n2) -> (OK, Bool(n1=n2)) 
      | _ -> (Error, tm)
    else if is_val_1 then (* Eq-2 *)
      match step t2 with
      | (Error, tm) -> (Error, tm)
      | (OK, t2') -> (OK, Eq(t1, t2'))
    else (* Eq-1 *)
      match step t1 with
      | (Error, tm) -> (Error, tm)
      | (OK, t1') -> (OK, Eq(t1', t2))
  )

  | If(t1, t2, t3) -> (
    (*
      最后是 If 的规则, 这与之前的规则有较大差别, 请按照规则完成:
         t1 --> t1'
      -------------------------------- If-1
      if t1 then t2 else t3 end
        --> if t1' then t2 else t3 end

         t1 val t1=Bool(true)
      -------------------------------- If-2
      if t1 then t2 else t3 end --> t2

         t1 val t1=Bool(false)
      -------------------------------- If-3
      if t1 then t2 else t3 end --> t3
     *)
    let is_val_1 = value t1 in
    if is_val_1 then 
      match t1 with 
      | Bool(true) -> (OK, t2)  (* If-2 *)
      | Bool(false) -> (OK, t3) (* If-3 *)
      | _ -> (Error, tm)
    else (* If-1 *)
      match step t1 with
      | (Error, tm) -> (Error, tm)
      | (OK, t1') -> (OK, If(t1', t2, t3))
  )
  | Fix(bind, body) -> 
    (* 
      这一分支是 fix 拓展部分, 请放到最后完成.
      
      --------------------------------------------------- Fix
        fix bind = body --> [fix bind = body/bind] body

     *)
    (OK, subst_free (Fix(bind, body)) bind body)
    (* (OK, body) *)


(*
  你可以在 `$ dune utop` 里试着使用 step, 例如:
  `
  let (stat, res) = {| (\x.3) (4+5) |} |> Parse.tm_nl |> Dynamics.step 
  `
 *)

(*
  消灭 Todo.Dynmaics 之后, 让我们把目光转移到动态语义出错上来.
  形如 true + 3 --> 出错, 我们知道, 这是因为 true 与 3 不能做加法.
  形如 true + 3 这样简单的项, 我们很容易肉眼检查出错误,
  但当程序复杂之后, 就有可能在某个云深不知处藏了这样的错误, 
  可能在某次程序执行, 这样不为人知的错误带来了严重的后果. 
  事实上, 曾经 Bilibili 就出现一次服务器宕机事件, 原因正是这种深藏的错误.

  为了能尽量减少运行时错误, 类型系统与类型检查孕育而生.
  接下来你应前往 `type.ml` 去了解类型的抽象语法树结构,
  以及前往 `statics.ml` 去解决静态语义问题.
 *)