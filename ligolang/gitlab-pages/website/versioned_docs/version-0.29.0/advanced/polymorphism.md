---
id: polymorphism
title: Polymorphism
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

LIGO supports simple polymorphism when introducing declarations. This
allows to write functions parametric on a type that can be later
instantiated to concrete types.

## The identity function

For any given type `t`, there is a canonical function of type `t -> t`
(function from `t` to `t`): it takes an argument, and returns it
immediately. For instance, we can write the identity function for
`int` as follows:

<Syntax syntax="pascaligo">

```pascaligo group=mono
function id (const x : int) : int is
  x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=mono
let id (x : int) = x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=mono
let id = (x : int) : int => x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=mono
let id = (x: int): int => x;
```

</Syntax>

However, if we would want to use the same function on a different
type, such as `nat`, we will need to write a new definition:

<Syntax syntax="pascaligo">

```pascaligo group=mono
function idnat (const x : nat) : nat is
  x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=mono
let idnat (x : nat) = x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=mono
let idnat = (x : nat) : nat => x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=mono
let idnat = (x: nat): nat => x;
```

</Syntax>

If we see in detail, there is almost no difference between `id` and
`idnat`: it's just the type that changes, but for the rest, the body
of the function remains the same.

Thanks to parametric polymorphism, we can write a single function
declaration that works for both cases.

<Syntax syntax="pascaligo">

```pascaligo group=poly
function id (const x : _a) : _a is
  x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=poly
let id (type a) (x : a) : a = x
```

Here we introduce an abstract type variable `a` which can be
generalised using `(type a)` in the declaration. Alternatively, we
could write:

```cameligo group=poly2mligo
let id (x : _a) : _a = x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=poly
let id : (_a => _a) = (x : _a) => x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=poly
let id : ((x : _a) => _a) = (x: _a) => x;
```

</Syntax>

Here `_a` is a type variable which can be generalised. In general,
types prefixed with `_` are treated as generalisable.

We can then use this function directly in different types by just
regular application:

<Syntax syntax="pascaligo">

```pascaligo group=poly
const three_i : int = id(3);
const three_s : string = id("three");
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=poly
let three_i : int = id 3
let three_s : string = id "three"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=poly
let three_i : int = id(3);
let three_s : string = id("three");
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=poly
const three_i : int = id(3);
const three_s : string = id("three");
```

</Syntax>

During compilation, LIGO will monomorphise the polymorphic functions
into specific instances, resulting in Michelson code that does not
contain polymorphic function declarations anymore.

## Polymorphism with parametric types

Polymorphism is especially useful when writing functions over
parametric types, which include built-in types like lists, sets, and
maps.

As an example, we will see how to implement list reversing
parametrically on any type, rather than just on lists of a specific
type.

Similar to the `id` example, we can introduce a type variable that can
be generalised. We will write a direct version of the function using
an accumulator, but the reader can experiment with different
variations by using `List` combinators.

<Syntax syntax="pascaligo">

```pascaligo group=poly
function rev(const xs : list (_a)) : list (_a) is block {
  var acc := (nil : list (_a));
  for x in list xs block {
    acc := x # acc;
  };
} with acc
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=poly
let rev (type a) (xs : a list) : a list =
  let rec rev (type a) ((xs, acc) : a list * a list) : a list =
    match xs with
    | [] -> acc
    | x :: xs -> rev (xs, (x :: acc)) in
  rev (xs, ([] : a list))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=poly
let rev : (list (_a) => list (_a)) = (xs : list (_a)) : list (_a) =>
  let rec rev  : ((list (_a), list (_a)) => list (_a)) = ((xs, acc) : (list (_a), list (_a))) : list (_a) =>
    switch xs {
    | [] => acc
    | [x,... xs] => rev (xs, [x,... acc])
    };
  rev (xs, ([] : list (_a)));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=poly
let rev : ((xs : list<_a>) => list<_a>) = (xs : list<_a>) : list<_a> => {
  let _rev  : ((p : [list<_a>, list<_a>]) => list<_a>) = ([xs, acc] : [list<_a>, list<_a>]) : list<_a> =>
    match(xs, list([
    ([] : list<_a>) => acc,
    ([x,... xs] : list<_a>) => _rev([xs, list([x,...acc])])
    ]));

  return _rev([xs, (list([]) as list<_a>)]);
};
```

</Syntax>

We use an accumulator variable `acc` to keep the elements of the list
processed, consing each element on it.

As with the identity function, we can then use it directly in
different types:

<Syntax syntax="pascaligo">

```pascaligo group=poly
const lint : list(int) = rev(list [1; 2; 3]);
const lnat : list(nat) = rev(list [1n; 2n; 3n]);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=poly
let lint : int list = rev [1; 2; 3]
let lnat : nat list = rev [1n; 2n; 3n]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=poly
let lint : list (int) = rev([1, 2, 3]);
let lnat : list (nat) = rev([1n, 2n, 3n]);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=poly
const lint : list<int> = rev(list([1, 2, 3]));
const lnat : list<nat> = rev(list([(1 as nat), (2 as nat), (3 as nat)]));
```

</Syntax>
