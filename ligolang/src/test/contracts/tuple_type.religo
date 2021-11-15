/*
  The difference between tuples and arguments is subtle in ReasonLIGO.

   `f(a, b);`
   f is called with two arguments

   `f((a, b));`
   f is called with a tuple.

*/

type fun_type = (int, int) => int;

let arguments = (b: int, c: int) => { b + c; };

let arguments_type_def = (b: fun_type) => b (5, 3);

let arguments_test = (_: int) => arguments_type_def (arguments);

type tuple_type = ((int, int)) => int;

let tuple = ((a, b): (int, int)) => { a + b; };

let tuple_type_def = (b: tuple_type) => b ((5, 3));

let tuple_test = (_: int) => tuple_type_def (tuple);


/* inline */

let arguments_inline = (b: int, c: int) => { b + c; };

let arguments_type_def_inline = (b: (int, int) => int) => b (5, 3);

let arguments_test_inline = (_: int) =>
  arguments_type_def_inline (arguments_inline);

let tuple_inline = ((a, b): (int, int)) => { a + b; };

let tuple_type_def_inline = (b: ((int, int)) => int) => b ((5, 3));

let tuple_test_inline = (_: int) =>
  tuple_type_def_inline(tuple_inline);
