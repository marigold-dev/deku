(* Test a PascaLIGO function which takes another function as an
   argument *)

function foobar (const _i : int) : int is
  block {
    function foo (const _i : int) : int is _i;
    function bar (const f : int -> int) : int is f (_i);
  } with bar (foo)

// higher order function with more than one argument

function higher2 (const _i : int; const f : int -> int): int is f (_i)

function foobar2 (const _i : int) : int is
  block {
    function foo2 (const _i : int) : int is _i
  } with higher2 (_i, foo2)

const a : int = 0

function foobar3 (const _i : int) : int is
  block {
    function foo2 (const _i : int) : int is a+_i
  } with higher2 (_i, foo2)

function f (const _i : int) : int is _i

function g (const _i : int) : int is f (_i)

function foobar4 (const _i : int) : int is g (g (_i))

function higher3 (const _i : int;
                  const f : int -> int;
                  const g : int -> int) : int is f (g (_i))

function foobar5 (const _i : int) : int is
  block {
    const a : int = 0;
    function foo (const _i : int) : int is a+_i;
    function goo (const _i : int) : int is foo (_i)
  } with higher3 (_i, foo, goo)

function foobar6 (const _i : int) : int -> int is f
