function mod_op   (const n : int) : nat is n mod 42
function plus_op  (const n : int) : int is n + 42
function minus_op (const n : int) : int is n - 42
function times_op (const n : int) : int is n * 42
function div_op   (const n : int) : int is n / 2
function int_op   (const n : nat) : int is int (n)
function neg_op   (const n : int) : int is -n
function foo      (const n : int) : int is n + 10
function neg_op_2 (const b : int) : int is -(foo (b))
function ediv_op  (const n : int) : option (int * nat) is ediv (n,2)
const mul_woo = block {
   const _x = 42 * 42n;
   const _y = 23n * 23;
   const _z = 144 * 144;
   const _w = 7n * 7n;
} with Unit
