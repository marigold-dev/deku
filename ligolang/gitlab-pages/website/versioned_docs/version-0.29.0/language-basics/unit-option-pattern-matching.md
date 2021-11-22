---
id: unit-option-pattern-matching
title: Unit, Option, Pattern matching
---

import Syntax from '@theme/Syntax';


Optionals are a pervasive programming pattern in OCaml. Since Michelson
and LIGO are both inspired by OCaml, *optional types* are available in
LIGO as well. Similarly, OCaml features a *unit* type, and LIGO
features it as well. Both the option type and the unit types are
instances of a more general kind of types: *variant types* (sometimes
called *sum types*).

## The unit Type

The `unit` type in Michelson or LIGO is a predefined type that
contains only one value that carries no information. It is used when
no relevant information is required or produced. Here is how it used.


<Syntax syntax="pascaligo">

In PascaLIGO, the unique value of the `unit` type is `Unit`.
```pascaligo group=a
const n : unit = Unit // Note the capital letter
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the unique value of the `unit` type is `()`, following
the OCaml convention.
```cameligo group=a
let n : unit = ()
```

Sequences of expressions that return the `unit` type can be written
using `begin` and `end`, separating expressions using semi-colons. The
last expression, which represents the value returned, can have a
different type to `unit`:

```cameligo group=a
let m (x : int) =
  begin
    assert (x > 0);
    assert (x < 10);
    x
  end
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the unique value of the `unit` type is `()`, following
the OCaml convention.
```reasonligo group=a
let n : unit = ();
```

Sequences of expressions that return the `unit` type can be written
using braces, separating expressions using semi-colons. The last
expression, which represents the value returned, can have a different
type to `unit`:

```reasonligo group=a
let m = (x : int) =>
  { assert (x > 0);
    assert (x < 10);
    x }
```

</Syntax>
<Syntax syntax="jsligo">

In JsLIGO, the unique value of the `unit` type is `unit`.
```jsligo group=a
let n : unit = unit;
```

</Syntax>


## Variant types

A variant type is a user-defined or a built-in type (in case of
options) that defines a type by cases, so a value of a variant type is
either this, or that or... The simplest variant type is equivalent to
the enumerated types found in Java, C++, JavaScript etc.

Here is how we define a coin as being either head or tail (and nothing
else):


<Syntax syntax="pascaligo">

```pascaligo group=b
type coin is Head | Tail
const head : coin = Head
const tail : coin = Tail
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
type coin = Head | Tail
let head : coin = Head
let tail : coin = Tail
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
type coin = Head | Tail;
let head : coin = Head;
let tail : coin = Tail;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
type coin = ["Head"] | ["Tail"];
let head: coin = Head();
let tail: coin = Tail();
```

</Syntax>


The names `Head` and `Tail` in the definition of the type `coin` are
called *data constructors*, or *variants*. In this particular, they
carry no information beyond their names, so they are called *constant
constructors*.

In general, it is interesting for variants to carry some information,
and thus go beyond enumerated types. In the following, we show how to
define different kinds of users of a system.



<Syntax syntax="pascaligo">

```pascaligo group=c
type id is nat

type user is
  Admin   of id
| Manager of id
| Guest

const u : user = Admin (1000n)
const g : user = Guest
```

In PascaLIGO, a constant constructor is equivalent to the same constructor
taking an argument of type `unit`, so, for example, `Guest` is the
same value as `Guest (unit)`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
type id = nat

type user =
  Admin   of id
| Manager of id
| Guest

let u : user = Admin 1000n
let g : user = Guest
```

In CameLIGO, a constant constructor is equivalent to the same constructor
taking an argument of type `unit`, so, for example, `Guest` is the
same value as `Guest ()`.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
type id = nat;

type user =
| Admin   (id)
| Manager (id)
| Guest;

let u : user = Admin (1000n);
let g : user = Guest;
```

In ReasonLIGO, a constant constructor is equivalent to the same constructor
taking an argument of type `unit`, so, for example, `Guest` is the
same value as `Guest (unit)`.

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
type id = nat;

type user =
  ["Admin", id]
| ["Manager", id]
| ["Guest"];

let u : user = Admin(1000 as nat);
let g : user = Guest();
```


In JsLIGO, a constant constructor is equivalent to the same constructor
taking an argument of type `unit`, so, for example, `Guest ()` is the
same value as `Guest (unit)`.
</Syntax>


## Optional values

The `option` type is a predefined variant type that is used to express
whether there is a value of some type or none. This is especially
useful when calling a *partial function*, that is, a function that is
not defined for some inputs. In that case, the value of the `option`
type would be `None`, otherwise `Some (v)`, where `v` is some
meaningful value *of any type*. An example in arithmetic is the
division operation:


<Syntax syntax="pascaligo">

```pascaligo group=d
function div (const a : nat; const b : nat) : option (nat) is
  if b = 0n then (None: option (nat)) else Some (a/b)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=d
let div (a, b : nat * nat) : nat option =
  if b = 0n then (None: nat option) else Some (a/b)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=d
let div = ((a, b) : (nat, nat)) : option (nat) =>
  if (b == 0n) { (None: option (nat)); } else { Some (a/b); };
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=d
let div = ([a, b]: [nat, nat]): option<nat> => {
  if(b == (0 as nat)){ 
    return (None() as option <nat>); 
  } else { 
    return (Some (a/b)); 
  };
};
```

</Syntax>

You can extract the value of a `Some (v)` with the function `Option.unopt (Some (v))`. In case the value is `None`, this will fail with an error.

The proper way to deal with optional values is by means of pattern matching.


## Pattern matching

*Pattern matching* is similar to the `switch` construct in
JavaScript, and can be used to route the program's control flow based
on the value of a variant, record, tuple, or list.

A component of a pattern can be discarded by using a wildcard `_`
instead of a variable name.

LIGO will warn about unused variables bound in patterns in the same
way that function arguments are warned about. Variable names beginning
with `_` can be used as a binder to prevent warnings.

<Syntax syntax="reasonligo">

> Note: Support for pattern matching isn't yet stable for ReasonLIGO.

</Syntax>

<Syntax syntax="jsligo">

> Note: JsLIGO only supports basic pattern matching at the moment. This will change in the future.

</Syntax>

### Match on variants

Here is a function that transforms a colour variant type to an int.

<Syntax syntax="pascaligo">

```pascaligo group=pm_variant
type color is
  | RGB   of int * int * int
  | Gray  of int 
  | Default

function int_of_color (const c : color) : int is
  case c of
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray (i) -> 232 + i
  | Default -> 0
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=pm_variant
type color =
  | RGB   of int * int * int
  | Gray  of int 
  | Default

let int_of_color (c : color) : int =
  match c with
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i
  | Default -> 0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=pm_variant
type color =
  | RGB  ((int, int, int))
  | Gray (int)
  | Default

let int_of_color = (c : color) : int =>
  switch (c) {
  | RGB (r,g,b) => (16 + b + g * 6 + r * 36)
  | Gray i => 232 + i
  | Default => 0
  };
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=pm_variant
type color =
| ["RGB", [int, int, int]]
| ["Gray", int]
| ["Default"];

let int_of_color = (c : color) : int =>
  match(c, {
    RGB: (rgb : [int,int,int]) => 16 + rgb[2] + rgb[1] * 6 + rgb[0] * 36,
    Gray: (i : int) => 232 + i,
    Default: () => 0 });
```

</Syntax>

### Match on records or tuples

Fields of records and components of tuples can be destructured. Record pattern variables can be renamed.


<Syntax syntax="cameligo">

```cameligo group=pm_rec_tuple
type my_record = { a : int ; b : nat ; c : string }
type my_tuple = int * nat * string

let on_record (v : my_record) : int =
  match v with
  | { a ; b = b_renamed ; c = _ } -> a + int(b_renamed)

let on_tuple (v : my_tuple) : int =
  match v with
  | ( x , y , _ ) -> x + int(y)
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo group=pm_rec_tuple
type my_record is record [ a : int ; b : nat ; c : string ]
type my_tuple is (int * nat * string)

function on_record (const v : my_record) : int is
  case v of
   record [ a ; b = b_renamed ; c = _ ] -> a + int(b_renamed)
  end

function on_tuple (const v : my_tuple) : int is
  case v of
  | ( x , y , _ ) -> x + int(y)
  end
```

</Syntax>
<Syntax syntax="reasonligo">

<!-- skipping, REASONLIGO LEFTOVER -->
```reasonligo skip
type my_record = { a : int , b : nat , c : string } ;
type my_tuple = ( int , nat , string ) ;

let on_record = (v : my_record) : int =>
  switch v {
  | { a , b : b_renamed , c : _ } => a + int(b_renamed)
  };

let on_tuple = (v : my_tuple) : int =>
  switch v {
  | (x,y,_) => x + int(y)
  };
```

</Syntax>
<Syntax syntax="jsligo">

Pattern-matching on records and tuples are not supported in JsLIGO yet.

</Syntax>

### Match on lists

<Syntax syntax="cameligo">

```cameligo group=pm_lists
let weird_length (v : int list) : int =
  match v with 
  | [] -> -1
  | [ a; b ; c] -> -2
  | x -> int (List.length x)
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo group=pm_lists
function weird_length (const v : list(int)) : int is
  case v of 
  | nil -> -1
  | list [ a; b ; c] -> -2
  | x -> int (List.length (x))
  end
```

</Syntax>
<Syntax syntax="reasonligo">

<!-- skipping, REASONLIGO LEFTOVER -->
```reasonligo skip
let weird_length = (v : list(int)) : int =>
  switch v {
  | [] => 1
  | [a,b,c] => -2
  | x => int (List.length (x))
  }

```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=pm_lists
let weird_length = (v : list<int>) : int =>
  match(v, list([
    ([] : list<int>) => -1,
    ([hd, ...tl] : list<int>) => 1 + int(List.length(tl))
  ]));
```

</Syntax>


### Deep patterns

Pattern matching can also be used for nested patterns.

<Syntax syntax="cameligo">

```cameligo group=pm_complex
type complex_t = { a : int list option ; b : int list }


let complex = fun (x:complex_t) (y:complex_t) ->
  match (x,y) with
  | {a=None;b=_} , { a = _ ; b = _ } -> -1
  | {a=_;b=_} , { a = Some ([]) ; b = (hd::tl) } -> hd
  | {a=_;b=_} , { a = Some (hd::tl) ; b = [] } -> hd
  | {a=Some a;b=_} , _ -> int (List.length a)
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo group=pm_complex
type complex_t is record [ a : option(list(int)) ; b : list(int) ]

function complex (const x:complex_t ; const y:complex_t) is
  case (x,y) of
  | (record [ a=None;b=_] , record [ a = _ ; b = _ ]) -> -1
  | (record [ a=_;b=_]    , record [ a = Some (nil) ; b = (hd#tl) ]) -> hd
  | (record [ a=_;b=_]    , record [ a = Some ((hd#tl)) ; b = nil ]) -> hd
  | (record [ a=Some (a);b=_] , _) -> int ( List.length(a) )
  end
```

</Syntax>
<Syntax syntax="reasonligo">

<!-- skipping, REASONLIGO LEFTOVER -->
```reasonligo skip
// let t13 = 
//   ((x: recordi) => 
//      ((y: recordi) => 
//         switch (x, y) {
//         | {a : None, b : _ }, {a : _, b : _ } => -1
//         | { a : _, b : _ }, {a : Some [], b : [hd, ...tl] } =>
//             hd
//         | { a : _, b = _ }, {a : Some [hd, ...tl], b : [] } =>
//             hd
//         | { a : Some a, b : _}, _ => int(Bytes.length(a))
//         }));
```

</Syntax>
