/* Test ReasonLIGO arithmetic operators */

let mod_op   = (n : int) : nat => n mod 42;
let plus_op  = (n : int) : int => n + 42;
let minus_op = (n : int) : int => n - 42;
let times_op = (n : int) : int => n * 42;
let div_op   = (n : int) : int => n / 2;
let neg_op   = (n : int) : int => - n;
let foo      = (n : int) : int => n + 10;
let neg_op_2 = (b : int) : int => -foo(b);
let ediv_op  = (n : int) : option ((int, nat)) => ediv (n,2) 
