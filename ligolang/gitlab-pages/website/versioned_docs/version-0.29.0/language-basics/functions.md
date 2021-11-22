---
id: functions
title: Functions
---

import Syntax from '@theme/Syntax';

LIGO functions are the basic building block of contracts. For example,
entrypoints are functions and each smart contract needs a main
function that dispatches control to the entrypoints (it is not already
the default entrypoint).

The semantics of function calls in LIGO is that of a *copy of the
arguments but also of the environment*. In the case of PascaLIGO, this
means that any mutation (assignment) on variables outside the scope of
the function will be lost when the function returns, just as the
mutations inside the functions will be.

## Declaring Functions


<Syntax syntax="pascaligo">

There are two ways in PascaLIGO to define functions: with or without a
*block*.

### Blocks

In PascaLIGO, *blocks* enable the sequential composition of
instructions into an isolated scope. Each block needs to include at
least one instruction.

```pascaligo skip
block { a := a + 1 }
```

If we need a placeholder, we use the instruction `skip` which leaves
the state unchanged.  The rationale for `skip` instead of a truly
empty block is that it prevents you from writing an empty block by
mistake.

```pascaligo skip
block { skip }
```

Blocks are more versatile than simply containing instructions: they
can also include *declarations* of values, like so:

```pascaligo skip
block { const a : int = 1 }
```

Functions in PascaLIGO are defined using the `function` keyword
followed by their `name`, `parameters` and `return` type definitions.

Here is how you define a basic function that computes the sum of two
integers:

```pascaligo group=a
function add (const a : int; const b : int) : int is
  block {
    const sum : int = a + b
  } with sum
```

The function body consists of two parts:

- `block { <instructions and declarations> }` is the logic of the function;
- `with <value>` is the value returned by the function.

By default, LIGO will warn about unused parameters inside
functions. In case we do not use a parameter, we can use the wildcard
`_` to prevent warnings. Either use `_` instead of the parameter
identifier:

```pascaligo
function k (const x : int; const _ : int) is x
```

or use a parameter identifier starting with wildcard:

```pascaligo
function k (const x : int; const _y : int) is x
```

### Blockless functions

Functions that can contain all of their logic into a single
*expression* can be defined without the need of a block:

```pascaligo
function identity (const n : int) : int is block { skip } with n  // Bad! Empty block not needed!

function identity (const n : int) : int is n  // Blockless
```

The value of the expression is implicitly returned by the
function. Another example is as follows:

```pascaligo group=b
function add (const a: int; const b : int) : int is a + b
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/functions/blockless.ligo '(1,2)' --entry-point add
# Outputs: 3
```

</Syntax>
<Syntax syntax="cameligo">

Functions in CameLIGO are defined using the `let` keyword, like other
values. The difference is that a succession of parameters is provided
after the value name, followed by the return type. This follows OCaml
syntax. For example:
```cameligo group=c
let add (a : int) (b : int) : int = a + b
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/functions/blockless.mligo '(1,2)' --entry-point add
# Outputs: 3
```

CameLIGO is a little different from other syntaxes when it comes to
function parameters. In OCaml, functions can only take one
parameter. To get functions with multiple arguments like we are used
to in imperative programming languages, a technique called
[currying](https://en.wikipedia.org/wiki/Currying) is used.  Currying
essentially translates a function with multiple arguments into a
series of single argument functions, each returning a new function
accepting the next argument until every parameter is filled. This is
useful because it means that CameLIGO supports
[partial application](https://en.wikipedia.org/wiki/Partial_application).

Currying is however *not* the preferred way to pass function arguments
in CameLIGO.  While this approach is faithful to the original OCaml,
it is costlier in Michelson than naive function execution accepting
multiple arguments. Instead, for most functions with more than one
parameter, we should gather the arguments in a
[tuple](sets-lists-tuples.md) and pass the tuple in as
a single parameter.

Here is how you define a basic function that accepts two integers and
returns an integer as well:

```cameligo group=b
let add (a, b : int * int) : int = a + b             // Uncurried
let add_curry (a : int) (b : int) : int = add (a, b) // Curried
let increment : int -> int = add_curry 1             // Partial application
```

You can run the `increment` function defined above using the LIGO
compiler like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/functions/curry.mligo 5 --entry-point increment
# Outputs: 6
```

The function body is a single expression, whose value is returned.

By default, LIGO will warn about unused arguments inside
functions. In case we do not use an argument, we can use the wildcard
`_` to prevent warnings. Either use `_` instead of the argument
identifier:

```cameligo
let k (x : int) (_ : int) = x
```

or use an identifier starting with wildcard:

```cameligo
let k (x : int) (_y : int) = x
```

</Syntax>
<Syntax syntax="reasonligo">

Functions in ReasonLIGO are defined using the `let` keyword, like
other values. The difference is that a tuple of parameters is provided
after the value name, with its type, then followed by the return type.

Here is how you define a basic function that sums two integers:
```reasonligo group=b
let add = ((a, b): (int, int)) : int => a + b;
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/functions/blockless.religo '(1,2)' --entry-point add
# Outputs: 3
```

As in CameLIGO and with blockless functions in PascaLIGO, the function
body is a single expression, whose value is returned.

If the body contains more than a single expression, you use block
between braces:
```reasonligo group=b
let myFun = ((x, y) : (int, int)) : int => {
  let doubleX = x + x;
  let doubleY = y + y;
  doubleX + doubleY
};
```

By default, LIGO will warn about unused arguments inside
functions. In case we do not use an argument, we can use the wildcard
`_` to prevent warnings. Either use `_` instead of the argument
identifier:

```reasonligo
let k = ((x, _) : (int, int)) => x;
```

or use an identifier starting with wildcard:

```reasonligo
let k = ((x, _y) : (int, int)) => x;
```

</Syntax>
<Syntax syntax="jsligo">

Functions in JsLIGO are defined using the `let` or `const` keyword, like
other values. The difference is that parameters are provided
after the value name, with its type, then followed by the return type.

Here is how you define a basic function that sums two integers:

```jsligo group=b
let add = ([a, b]: [int, int]): int => a + b;
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/functions/blockless.jsligo '(1,2)' --entry-point add
# Outputs: 3
```



As in CameLIGO and with blockless functions in PascaLIGO, the function
body is a single expression, whose value is returned.

If the body contains more than a single expression, you use block
between braces:

```jsligo group=b
let myFun = ([x, y]: [int, int]): int => {
  let doubleX = x + x;
  let doubleY = y + y;
  return doubleX + doubleY;
};
```

Note that JsLIGO, like JavaScript, requires the `return` keyword to indicate 
what is being returned. If `return` is not used, it will be the same as 
`return unit`.

By default, LIGO will warn about unused arguments inside
functions. In case we do not use an argument, we can use the wildcard
`_` to prevent warnings. Either use `_` instead of the argument
identifier:

```jsligo
let k = ([x, _] : [int, int]) : int => x;
```

or use an identifier starting with wildcard:

```jsligo
let k_other = ([x, _y] : [int, int]) : int => x;
```

</Syntax>


## Anonymous functions (a.k.a. lambdas)

It is possible to define functions without assigning them a name. They
are useful when you want to pass them as arguments, or assign them to
a key in a record or a map.

Here is how to define an anonymous function:


<Syntax syntax="pascaligo">

```pascaligo group=c
function increment (const b : int) : int is
   (function (const a : int) : int is a + 1) (b)
const a : int = increment (1); // a = 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/functions/anon.ligo --entry-point a
# Outputs: 2
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let increment (b : int) : int = (fun (a : int) -> a + 1) b
let a : int = increment 1 // a = 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/functions/anon.mligo --entry-point a
# Outputs: 2
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let increment = (b : int) : int => ((a : int) : int => a + 1) (b);
let a : int = increment (1); // a == 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/functions/anon.religo --entry-point a
# Outputs: 2
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let increment = (b: int): int => ((a: int): int => a + 1) (b);
let a: int = increment(1); // a == 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/functions/anon.jsligo --entry-point a
# Outputs: 2
```

</Syntax>


If the example above seems contrived, here is a more common design
pattern for lambdas: to be used as parameters to functions. Consider
the use case of having a list of integers and mapping the increment
function to all its elements.


<Syntax syntax="pascaligo">

```pascaligo group=c
function incr_map (const l : list (int)) : list (int) is
  List.map (function (const i : int) : int is i + 1, l)
```

You can call the function `incr_map` defined above using the LIGO
compiler like so:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/functions/incr_map.ligo --entry-point incr_map
"list [1;2;3]"
# Outputs: [ 2 ; 3 ; 4 ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let incr_map (l : int list) : int list =
  List.map (fun (i : int) -> i + 1) l
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/functions/incr_map.mligo --entry-point incr_map
"list [1;2;3]"
# Outputs: [ 2 ; 3 ; 4 ]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let incr_map = (l : list (int)) : list (int) =>
  List.map ((i : int) => i + 1, l);
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/functions/incr_map.religo --entry-point incr_map
"list [1;2;3]"
# Outputs: [ 2 ; 3 ; 4 ]
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let incr_map = (l: list<int>): list<int> =>
  List.map((i: int) => i + 1, l);
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/functions/incr_map.jsligo --entry-point incr_map
"list [1;2;3]"
# Outputs: [ 2 ; 3 ; 4 ]
```

</Syntax>


## Nested functions (also known as closures)
It's possible to place functions inside other functions. These functions
have access to variables in the same scope. 

<Syntax syntax="pascaligo">

```pascaligo
function closure_example (const i : int) : int is
  block {
    function closure (const j : int) : int is i + j
  } with closure (i)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let closure_example (i : int) : int =
  let closure : int -> int = fun (j : int) -> i + j in
  closure i
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let closure_example = (i : int) : int => {
  let closure = (j: int): int => i + j;
  closure(i);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let closure_example = (i: int): int => {
  let closure = (j: int): int => i + j;
  return closure(i);
};
```

</Syntax>



<Syntax syntax="pascaligo">

## Recursive function

LIGO functions are not recursive by default, the user need to indicate that the function is recursive.

At the moment, recursive function are limited to one (possibly tupled) parameter and recursion is
limited to tail recursion (i.e the recursive call should be the last expression of the function).

In PascaLIGO recursive functions are defined using the `recursive` keyword

```pascaligo group=d
recursive function sum (const n : int; const acc: int) : int is
  if n<1 then acc else sum(n-1,acc+n)

recursive function fibo (const n: int; const n_1: int; const n_0 :int) : int is
  if n<2 then n_1 else fibo(n-1,n_1+n_0,n_1)
```
</Syntax>
<Syntax syntax="cameligo">

## Recursive function

LIGO functions are not recursive by default, the user need to indicate that the function is recursive.

At the moment, recursive function are limited to one (possibly tupled) parameter and recursion is
limited to tail recursion (i.e the recursive call should be the last expression of the function)

In CameLIGO recursive functions are defined using the `rec` keyword

```cameligo group=d
let rec sum ((n,acc):int * int) : int =
    if (n < 1) then acc else sum (n-1, acc+n)
 
let rec fibo ((n,n_1,n_0):int*int*int) : int = 
    if (n < 2) then n_1 else fibo (n-1, n_1 + n_0, n_1)
```
</Syntax>
<Syntax syntax="reasonligo">

## Recursive function

LIGO functions are not recursive by default, the user need to indicate that the function is recursive.

At the moment, recursive function are limited to one (possibly tupled) parameter and recursion is
limited to tail recursion (i.e the recursive call should be the last expression of the function)

In ReasonLIGO recursive functions are defined using the `rec` keyword

```reasonligo group=d
let rec sum = ((n, acc) : (int,int)): int =>
    if (n < 1) {acc;} else {sum ((n-1,acc+n));};

let rec fibo = ((n, n_1, n_0) : (int,int,int)): int =>
    if (n < 2) {n_1;} else {fibo ((n-1,n_1+n_0,n_1));};
```
</Syntax>