/* Test a function which takes another function as an argument */

let foobar = (i : int): int => {
  let foo: int => int = (i: int) => i;
  let bar: ((int => int) => int) = (f : (int => int)) => f (i);
  bar (foo);
};

/* higher order function with more than one argument */

let higher2 = (i : int, f : (int => int)) : int => {
  let ii : int = f (i);
  ii;
};

let foobar2 = (i : int) : int => {
  let foo2 : int => int = (i : int) => i;
  higher2 (i, foo2);
};

let a : int = 0;

let foobar3 = (i : int) : int => {
  let foo2: int => int = (i : int) => a + i;
  higher2 (i, foo2);
};

let f = (i : int) : int => i;

let g = (i : int) : int => f (i);

let foobar4 = (i : int) : int => g (g (i));

let higher3 = (i : int, f : (int => int), g : (int => int)) : int => {
  let ii : int = f (g (i));
  ii;
};

let foobar5 = (i : int) : int => {
  let a : int = 0;
  let foo : int => int = (i : int) => a + i;
  let goo : int => int = (i : int) => foo (i);
  higher3 (i, foo, goo);
};
