(* Some basic types and functions *)
type some_r = [@layout:comb] { one : int ; two : nat ; three : string ; four : bytes ; five : unit }
type some_v = [@layout:comb] | Foo of int | Bar of string | Bare of string | Baz of nat
let f = fun (x:some_r) -> x.one

type some_r_2 = [@layout:tree] { one2 : int ; two2 : nat ; three2 : string ; four2 : bytes ; five2 : unit }
type some_v_2 = [@layout:tree] | Foo2 of int | Bar2 of string | Bare2 of string | Baz2 of nat
let f_2 = fun (x:some_r_2) -> x.one2

let test1 =
  let d_one = Test.eval (1 + 3 + 2) in
  let ret = Test.run (fun (x : (int * nat * string * bytes * unit)) -> f ({ one = x.0 ; two = x.1 ; three = x.2 ; four = x.3 ; five = x.4 } : some_r))
                   (1 + 3 + 2, 1n + 2n, "a" ^ "b", 0xFF00, ()) in
  assert (Test.michelson_equal d_one ret)

let test2 =
  let x1 = Test.eval (1,2) in
  let x2 = Test.run (fun (x : int * int) -> x) (1, 2) in
  let eq = Test.michelson_equal x1 x2 in
  assert eq

let test3 =
  let x1 = Test.eval (Baz 1n : some_v) in
  let x2 = Test.eval (Baz2 1n : some_v_2) in
  assert (not (Test.michelson_equal x1 x2))

let test4 =
  let x1 = Test.eval ({ one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = () } : some_r) in
  let x2 = Test.eval ({ one2 = 1 ; two2 = 2n ; three2 = "a" ; four2 = 0xFF00 ; five2 = () } : some_r_2) in
  assert (not (Test.michelson_equal x1 x2))

