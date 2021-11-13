let foo = (a : int, b : int) : int => a + b;

let baz = (i : int) : int => foo (i, i * 2);
