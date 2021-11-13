---
id: loops
title: Iteration
---

import Syntax from '@theme/Syntax';

## General Iteration



<Syntax syntax="pascaligo">

General iteration in PascaLIGO takes the shape of general loops, which
should be familiar to programmers of imperative languages as "while
loops". Those loops are of the form `while <condition> <block>`. Their
associated block is repeatedly evaluated until the condition becomes
true, or never evaluated if the condition is false at the start. The
loop never terminates if the condition never becomes true. Because we
are writing smart contracts on Tezos, when the condition of a "while"
loops fails to become true, the execution will run out of gas and stop
with a failure anyway.

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm:

```pascaligo group=a
function gcd (var x : nat; var y : nat) : nat is
  block {
    if x < y then {
      const z : nat = x;
      x := y; y := z
    }
    else skip;
    var r : nat := 0n;
    while y =/= 0n block {
      r := x mod y;
      x := y;
      y := r
    }
  } with x
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/gcd.ligo '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)' --entry-point gcd
# Outputs: +12
```

</Syntax>
<Syntax syntax="cameligo">

CameLIGO is a functional language where user-defined values are
constant, therefore it makes no sense in CameLIGO to feature loops,
which we understand as syntactic constructs where the state of a
stopping condition is mutated, as with "while" loops in PascaLIGO.

Instead, CameLIGO loops are written by means of a tail recursive function

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm:

```cameligo group=a
let rec iter (x,y : nat * nat) : nat =
  if y = 0n then x else iter (y, x mod y)

let gcd (x,y : nat * nat) : nat =
  let x,y = if x < y then y,x else x,y in
  iter (x,y)
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/gcd.mligo '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)' --entry-point gcd
# Outputs: +12
```

</Syntax>
<Syntax syntax="reasonligo">

ReasonLIGO is a functional language where user-defined values are
constant, therefore it makes no sense in ReasonLIGO to feature loops,
which we understand as syntactic constructs where the state of a
stopping condition is mutated, as with "while" loops in PascaLIGO.

Instead, ReasonLIGO loops are written by means of tail recursive functions

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm:

```reasonligo group=a
let rec iter = ((x,y) : (nat, nat)) : nat =>
  if (y == 0n) { x; } else { iter ((y, x mod y)); };

let gcd = ((x,y) : (nat, nat)) : nat => {
  let (x,y) = if (x < y) { (y,x); } else { (x,y); };
  iter ((x,y))
};
```

</Syntax>
<Syntax syntax="jsligo">

JsLIGO currently supports iteration through while while loops and
through the use of tail recursive functions.

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm with using a while loop:


```jsligo group=a
let gcd = (x: nat, y: nat): nat => {
  let [x,y] = [x,y]; // we will modify x and y
  if (x < y) {
    const z: nat = x;
    x = y; y = z;
  };
  let r: nat = 0 as nat;
  while (y != (0 as nat)) {
    r = x % y;
    x = y;
    y = r;
  }
  return x
}
```

And here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm using tail recursion:

```jsligo group=a
let iter = ([x,y]: [nat, nat]): nat => {
  if (y == (0 as nat)) { 
    return x; 
  } else { 
    return iter ([y, x % y]); 
  };
};

let gcd2 = ([x,y] : [nat, nat]) : nat => {
  if (x < y) { 
    return iter ([y, x]);
  } else {
    return iter ([x, y]);
  }
};
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/gcd.jsligo '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)' --entry-point gcd
# Outputs: +12
```

and can call the function `gcd2` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/gcd.jsligo '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)' --entry-point gcd2
# Outputs: +12
```

</Syntax>

<Syntax syntax="pascaligo">

## Bounded Loops

In addition to general loops, PascaLIGO features a specialised kind of
*loop to iterate over bounded intervals*. These loops are familiarly
known as "for loops" and they have the form `for <variable assignment>
to <upper bound> <block>`, as found in imperative languages.

Consider how to sum the natural numbers up to `n`:

```pascaligo group=c
function sum (var n : nat) : int is block {
  var acc : int := 0;
  for i := 1 to int (n) block {
    acc := acc + i
  }
} with acc
```

(Please do not use that function: there exists a closed form formula.)

You can call the function `sum` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/sum.ligo 7n --entry-point sum
# Outputs: 28
```

PascaLIGO "for" loops can also iterate through the contents of a
collection, that is, a list, a set or a map. This is done with a loop
of the form `for <element var> in <collection type> <collection var> <block>`, 
where `<collection type>` is any of the following keywords:
`list`, `set` or `map`.

Here is an example where the integers in a list are summed up.

```pascaligo group=d
function sum_list (var l : list (int)) : int is block {
  var total : int := 0;
  for i in list l block {
    total := total + i
  }
} with total
```

You can call the function `sum_list` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.ligo --entry-point sum_list
'list [1;2;3]'
# Outputs: 6
```

Here is an example where the integers in a set are summed up.

```pascaligo group=d
function sum_set (var s : set (int)) : int is block {
  var total : int := 0;
  for i in set s block {
    total := total + i
  }
} with total
```

You can call the function `sum_set` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.ligo --entry-point sum_set
'set [1;2;3]'
# Outputs: 6
```

Loops over maps are actually loops over the bindings of the map, that
is, a pair key-value noted `key -> value` (or any other
variables). Given a map from strings to integers, here is how to sum
all the integers and concatenate all the strings.

Here is an example where the keys are concatenated and the values are
summed up.

```pascaligo group=d
function sum_map (var m : map (string, int)) : string * int is block {
  var string_total : string := "";
  var int_total : int := 0;
  for key -> value in map m block {
    string_total := string_total ^ key;
    int_total := int_total + value
  }
} with (string_total, int_total)
```

You can call the function `sum_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.ligo --entry-point sum_map
'map ["1"->1; "2"->2; "3"->3]'
# Outputs: ( "123", 6 )
```

</Syntax>
<Syntax syntax="jsligo">

## for-of Loops


JsLIGO "for-of" loops can iterate through the contents of a
collection, that is, a list, a set or a map. This is done with a loop
of the form `for (const <element var> of <collection var>) <block>`.

Here is an example where the integers in a list are summed up.

```jsligo group=d
let sum_list = (l: list<int>): int => {
  let total : int = 0;
  for (const i of l) {
    total = total + i
  }
  return total
} 
```

You can call the function `sum_list` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.jsligo --entry-point sum_list
'list [1;2;3]'
# Outputs: 6
```

Here is an example where the integers in a set are summed up.

```jsligo group=d
let sum_set = (s: set<int>): int => {
  let total : int = 0;
  for (const i of s) {
    total = total + i
  };
  return total
}
```

You can call the function `sum_set` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.jsligo --entry-point sum_set
'set [1;2;3]'
# Outputs: 6
```

Loops over maps are actually loops over the bindings of the map.
Given a map from strings to integers, here is how to sum
all the integers and concatenate all the strings.


```jsligo skip
let sum_map = (m: map<string, int>): [string, int] => {
  let string_total: string = "";
  let int_total: int = 0;
  for (const item of m) {
    let [key, value] = item;
    string_total = string_total + key;
    int_total = int_total + value
  };
  return [string_total, int_total]
}
```

> In the experimental version of JsLIGO looping over maps is not supported.
> Use `Map.iter` instead.

You can call the function `sum_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.jsligo --entry-point sum_map
'map ["1"->1; "2"->2; "3"->3]'
# Outputs: ( "123", 6 )
```

</Syntax>
