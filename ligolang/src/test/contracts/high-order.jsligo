/* Test a function which takes another function as an argument */

let foobar = (i : int): int => {
  let foo: (a: int) => int = (i: int) => i;
  let bar: ((a:((a: int) => int)) => int) = (f : ((a: int) => int)) => f (i);
  return bar (foo);
};

/* higher order function with more than one argument */

let higher2 = (i : int, f : ((i: int) => int)) : int => {
  let ii : int = f (i);
  return ii;
};

let foobar2 = (i : int) : int => {
  let foo2 : ((a: int) => int) = (i : int) => i;
  return higher2 (i, foo2);
};

let a : int = 0;

let foobar3 = (i : int) : int => {
  let foo2: (a: int) => int = (i : int) => a + i;
  return higher2 (i, foo2);
};

let f = (i : int) : int => i;

let g = (i : int) : int => f (i);

let foobar4 = (i : int) : int => g (g (i));

let higher3 = (i : int, f : ((a:int) => int), g : ((a: int) => int)) : int => {
  let ii : int = f (g (i));
  return ii;
};

let foobar5 = (i : int) : int => {
  const a : int = 0;
  const foo : (a: int) => int = (i : int) => a + i;
  let goo : (a: int) => int = (i : int) => foo (i);
  return higher3 (i, foo, goo);
};
