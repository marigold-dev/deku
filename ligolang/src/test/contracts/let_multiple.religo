/* Simple test of binding multiple values */

let ((x : int), (y : int)) = (1,2);

let main = (_ : unit): int => x + y;

let ((x : int), (y : int)) = (3,3);

let main_paren = (_ : unit): int => x + y;

let foobar : (int, int) = (23, 42);

let ((foo : int), (bar : int)) = foobar;

let non_tuple_rhs = (_ : unit) : int => foo + bar;
