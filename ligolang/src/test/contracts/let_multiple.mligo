(* Simple test of binding multiple values *)

let (x : int), (y : int) = 1,2

let main (_ : unit) : int = x + y

let ((x : int) , (y : int)) = 3,3

let main_paren (_ : unit) : int = x + y

let foobar : (int * int) = (23 , 42)
let (foo : int) , (bar : int) = foobar

(* Here to prevent a regression of https://gitlab.com/ligolang/ligo/issues/63#note_254106580 *)

let correct_values_bound (_ : unit) : int * int = foo, bar

let non_tuple_rhs (_ : unit) : int = bar - foo

(* Here to prevent a regression of https://gitlab.com/ligolang/ligo/issues/63#note_254106580 *)

let big_tuple : int * int * int * int * int = 10, 20, 30, 40, 50

let (a: int), (b: int), (c: int), (d: int), (e: int) = big_tuple

let correct_values_big_tuple (_ : unit) : int * int * int * int * int =
  a, b, c, d, e

(* Here to prevent a regression of https://gitlab.com/ligolang/ligo/issues/63#note_254106580 *)

let different_types : int * string = 10, "hello"

let (greet_num : int), (greeting : string) = different_types

let correct_values_different_types (_ : unit) : int * string =
  greet_num, greeting
