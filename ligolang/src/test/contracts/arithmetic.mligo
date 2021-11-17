let mod_op   (n : int) : nat = n mod 42
let plus_op  (n : int) : int = n + 42
let minus_op (n : int) : int = n - 42
let times_op (n : int) : int = n * 42
let div_op   (n : int) : int = n / 2
let neg_op   (n : int) : int = -n
let foo      (n : int) : int = n + 10
let neg_op_2 (b : int) : int = -(foo b)
let ediv_op  (n : int) : (int * nat) option = ediv n 2
let mul_woo =
  let _x : int = 42 * 42n in
  let _y : int = 23n * 23 in
  let _z : int = 144 * 144 in
  let _w : nat = 7n * 7n in
  ()
