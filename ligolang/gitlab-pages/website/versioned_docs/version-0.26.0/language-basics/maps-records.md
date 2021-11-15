---
id: maps-records
title: Records and Maps
---

import Syntax from '@theme/Syntax';

So far, we have seen pretty basic data types. LIGO also offers more
complex built-in constructs, such as *records* and *maps*.

## Records

Records are one-way data of different types can be packed into a
single type. A record is made of a set of *fields*, which are made of
a *field name* and a *field type*. Given a value of a record type, the
value bound to a field can be accessed by giving its field name to a
special operator (`.`).

Let us first consider an example of record type declaration.

<Syntax syntax="pascaligo">

```pascaligo group=records1
type user is
  record [
    id       : nat;
    is_admin : bool;
    name     : string
  ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=records1
type user = {
  id       : nat;
  is_admin : bool;
  name     : string
}
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=records1
type user = {
  id       : nat,
  is_admin : bool,
  name     : string
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=records1
type user = {
  id       : nat,
  is_admin : bool,
  name     : string
};
```

</Syntax>

And here is how a record value is defined:

<Syntax syntax="pascaligo">

```pascaligo group=records1
const alice : user =
  record [
    id       = 1n;
    is_admin = True;
    name     = "Alice"
  ]
```

> Notice that since `alice` is declared as a `const`, none of its fields can be updated.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=records1
let alice : user = {
  id       = 1n;
  is_admin = true;
  name     = "Alice"
}
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=records1
let alice : user = {
  id       : 1n,
  is_admin : true,
  name     : "Alice"
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=records1
let alice : user = {
  id       : 1 as nat,
  is_admin : true,
  name     : "Alice"
};
```

</Syntax>


### Accessing Record Fields

If we want the contents of a given field, we use the (`.`) infix
operator, like so:


<Syntax syntax="pascaligo">

```pascaligo group=records1
const alice_admin : bool = alice.is_admin
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=records1
let alice_admin : bool = alice.is_admin
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=records1
let alice_admin : bool = alice.is_admin;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=records1
let alice_admin: bool = alice.is_admin;
```

</Syntax>


### Functional Updates

Given a record value, it is a common design pattern to update only a
small number of its fields. Instead of copying the fields that are
unchanged, LIGO offers a way to only update the fields that are
modified.

One way to understand the update of record values is the *functional
update*. The idea is to have an *expression* whose value is the
updated record.

Let us consider defining a function that translates three-dimensional
points on a plane.

<Syntax syntax="pascaligo">

In PascaLIGO, the shape of that expression is
`<record variable> with <record value>`.
The record variable is the record to update, and the
record value is the update itself.

```pascaligo group=records2
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

const origin : point = record [x = 0; y = 0; z = 0]

function xy_translate (var p : point; const vec : vector) : point is
  p with record [x = p.x + vec.dx; y = p.y + vec.dy]
```

You can call the function `xy_translate` defined above by running the
following command of the shell:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_update.ligo
"(record [x=2;y=3;z=1], record [dx=3;dy=4])" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

> You have to understand that `p` has not been changed by the functional
> update: a nameless new version of it has been created and returned by
> the block-less function.

</Syntax>
<Syntax syntax="cameligo">

The syntax for the functional updates of record in CameLIGO follows
that of OCaml:

```cameligo group=records2
type point = {x : int; y : int; z : int}
type vector = {dx : int; dy : int}

let origin : point = {x = 0; y = 0; z = 0}

let xy_translate (p, vec : point * vector) : point =
  {p with x = p.x + vec.dx; y = p.y + vec.dy}
```

You can call the function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_update.mligo
"({x=2;y=3;z=1}, {dx=3;dy=4})" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

> You have to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

</Syntax>
<Syntax syntax="reasonligo">

The syntax for the functional updates of record in ReasonLIGO follows
that of ReasonML:

```reasonligo group=records2
type point = {x : int, y : int, z : int};
type vector = {dx : int, dy : int};

let origin : point = {x : 0, y : 0, z : 0};

let xy_translate = ((p, vec) : (point, vector)) : point =>
  {...p, x : p.x + vec.dx, y : p.y + vec.dy};
```

You can call the function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_update.religo
"({x:2,y:3,z:1}, {dx:3,dy:4})" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

> You have to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

</Syntax>
<Syntax syntax="jsligo">

The syntax for the functional updates of record in JsLIGO:

```jsligo group=records2
type point = {x: int, y: int, z: int};
type vector = {dx: int, dy: int};

let origin: point = {x: 0, y: 0, z: 0};

let xy_translate = ([p, vec]: [point, vector]): point =>
  ({...p, x: p.x + vec.dx, y: p.y + vec.dy});
```

You can call the function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_update.jsligo
"({x:2,y:3,z:1}, {dx:3,dy:4})" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

> You have to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

</Syntax>

#### Nested updates

<Syntax syntax="pascaligo">

A unique feature of LIGO is the ability to perform nested updates on records.

For example if you have the following record structure:

```pascaligo
type color is
| Blue
| Green

type preferences is record [
  color : color;
  other : int;
]

type account is record [
  id : int;
  preferences : preferences;
]
```

</Syntax>
<Syntax syntax="cameligo">

A unique feature of LIGO is the ability to perform nested updates on records.

For example if you have the following record structure:

```cameligo
type color =
  Blue
| Green

type preferences = {
  color : color;
  other : int;
}

type account = {
  id: int;
  preferences: preferences;
}
```

</Syntax>
<Syntax syntax="reasonligo">

A unique feature of LIGO is the ability to perform nested updates on records.

For example if you have the following record structure:

```reasonligo
type color =
  Blue
| Green;

type preferences = {
  color : color,
  other : int
}

type account = {
  id : int,
  preferences : preferences
}
```

</Syntax>
<Syntax syntax="jsligo">

A unique feature of LIGO is the ability to perform nested updates on records. 
JsLIGO however does not support the specialised syntax as the other syntaxes. 
The following however also does the trick.

For example if you have the following record structure:

```jsligo
type color =
  ["Blue"]
| ["Green"];

type preferences = {
  color: color,
  other: int
};

type account = {
  id: int,
  preferences: preferences
};
```

</Syntax>

You can update the nested record with the following code:

<Syntax syntax="pascaligo">

```pascaligo

function change_color_preference (var account : account; const color : color ) : account is
  block {
      account := account with record [preferences.color = color]
  } with account

```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let change_color_preference (account : account) (color : color) : account =
  { account with preferences.color = color }
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let change_color_preference = (account : account, color : color): account =>
  { ...account, preferences.color: color };
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let change_color_preference = (account : account, color : color): account =>
  ({ ...account, preferences: {...account.preferences, color: color }});
```

</Syntax>

Note that all the records in the path will get updated. In this example that's
`account` and `preferences`.

You can call the function `change_color_preference` defined above by running the
following command:

```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/maps-records/record_nested_update.ligo 
"(record [id=1001; preferences=record [color=Blue; other=1]], Green)" --entry-point change_color_preference
# Outputs: record[id -> 1001 , preferences -> record[color -> Green(unit) , other -> 1]]
```

<Syntax syntax="pascaligo">

### Record Patches

Another way to understand what it means to update a record value is to
make sure that any further reference to the value afterwards will
exhibit the modification. This is called a `patch` and this is only
possible in PascaLIGO, because a patch is an *instruction*, therefore
we can only use it in a block. Similarly to a *functional update*, a
patch takes a record to be updated and a record with a subset of the
fields to update, then applies the latter to the former (hence the
name "patch").

Let us consider defining a function that translates three-dimensional
points on a plane.

```pascaligo group=records3
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

function xy_translate (var p : point; const vec : vector) : point is
  block {
    patch p with record [x = p.x + vec.dx];
    patch p with record [y = p.y + vec.dy]
  } with p
```

You can call the function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_patch.ligo
"(record [x=2;y=3;z=1], record [dx=3;dy=4])" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

Of course, we can actually translate the point with only one `patch`,
as the previous example was meant to show that, after the first patch,
the value of `p` indeed changed. So, a shorter version would be

```pascaligo group=records4
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

function xy_translate (var p : point; const vec : vector) : point is
  block {
    patch p with record [x = p.x + vec.dx; y = p.y + vec.dy]
  } with p
```

You can call the new function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_patch2.ligo
"(record [x=2;y=3;z=1], record [dx=3;dy=4])" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

Record patches can actually be simulated with functional updates. All
we have to do is *declare a new record value with the same name as the
one we want to update* and use a functional update, like so:

```pascaligo group=records5
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

function xy_translate (var p : point; const vec : vector) : point is
  block {
    const p : point = p with record [x = p.x + vec.dx; y = p.y + vec.dy]
  } with p
```

You can call the new function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_simu.ligo
"(record [x=2;y=3;z=1], record [dx=3;dy=4])" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

The hiding of a variable by another (here `p`) is called `shadowing`.

</Syntax>

### Comparison

Record types are comparable, which allows to check for equality and
use records as key in sets or maps. By default, the ordering of
records is **undefined and implementation dependent**. Ultimately, the
order is determined by the translated Michelson type. When using the
`[@layout:comb]` attribute, fields are translated in their order in
the record, and records are then ordered with lexicographic ordering.

## Maps

*Maps* are a data structure which associate values of the same type to
values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

### Declaring a Map

Here is how a custom map from addresses to a pair of integers is
defined.

<Syntax syntax="pascaligo">

```pascaligo group=maps
type move is int * int
type register is map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
type move = int * int
type register = (address, move) map
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
type move = (int, int);
type register = map (address, move);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
type move = [int, int];
type register = map<address, move>;
```

</Syntax>


### Creating an Empty Map

Here is how to create an empty map.



<Syntax syntax="pascaligo">

```pascaligo group=maps
const empty : register = map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let empty : register = Map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let empty : register = Map.empty
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let empty: register = Map.empty;
```

</Syntax>


### Creating a Non-empty Map

And here is how to create a non-empty map value:


<Syntax syntax="pascaligo">

```pascaligo group=maps
const moves : register =
  map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

Notice the `->` between the key and its value and `;` to separate
individual map entries. The annotated value `("<string value>" :
address)` means that we cast a string into an address. Also, `map` is
a keyword.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let moves : register =
  Map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

The `Map.literal` predefined function builds a map from a list of
key-value pair tuples, `(<key>, <value>)`.  Note also the `;` to
separate individual map entries.  `("<string value>": address)` means
that we type-cast a string into an address. 

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let moves : register =
  Map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

The `Map.literal` predefined function builds a map from a list of
key-value pair tuples, `(<key>, <value>)`.  Note also the `,` to
separate individual map entries.  `("<string value>": address)` means
that we type-cast a string into an address. 

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let moves : register =
  Map.literal (list([
    ["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address, [1,2]],
    ["tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, [0,3]]]));
```

The `Map.literal` predefined function builds a map from a list of
key-value pair tuples, `[<key>, <value>]`.  Note also the `,` to
separate individual map entries.  `"<string value>" as address` means
that we type-cast a string into an address.

</Syntax>


### Accessing Map Bindings


<Syntax syntax="pascaligo">

In PascaLIGO, we can use the postfix `[]` operator to read the `move`
value associated to a given key (`address` here) in the register. Here
is an example:

```pascaligo group=maps
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let my_balance : move option =
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let my_balance : option (move) =
  Map.find_opt (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let my_balance: option<move> =
  Map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, moves);
```

</Syntax>


Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the map. This requires
*pattern matching*.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function force_access (const key : address; const moves : register) : move is
  case moves[key] of
    Some (move) -> move
  | None -> (failwith ("No move.") : move)
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let force_access (key, moves : address * register) : move =
  match Map.find_opt key moves with
    Some move -> move
  | None -> (failwith "No move." : move)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let force_access = ((key, moves) : (address, register)) : move => {
  switch (Map.find_opt (key, moves)) {
  | Some (move) => move
  | None => (failwith ("No move.") : move)
  }
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let force_access = ([key, moves]: [address, register]): move => {
  return match(Map.find_opt (key, moves), {
   Some: (move: register) => move,
   None: () => (failwith("No move.") as move)
  });
};
```

</Syntax>


### Updating a Map

Given a map, we may want to add a new binding, remove one, or modify
one by changing the value associated to an already existing key. All
those operations are called *updates*.



<Syntax syntax="pascaligo">

The values of a PascaLIGO map can be updated using the usual
assignment syntax `<map variable>[<key>] := <new value>`. Let us
consider an example.

```pascaligo group=maps
function assign (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=maps
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

See further for the removal of bindings.

</Syntax>
<Syntax syntax="cameligo">

We can update a binding in a map in CameLIGO by means of the
`Map.update` built-in function:

```cameligo group=maps
let assign (m : register) : register =
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) m
```

Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
use `None` instead, that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```cameligo group=maps
let add (m : register) : register =
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>
<Syntax syntax="reasonligo">

We can update a binding in a map in ReasonLIGO by means of the
`Map.update` built-in function:

```reasonligo group=maps
let assign = (m : register) : register =>
  Map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), Some ((4,9)), m);
```

Notice the optional value `Some (4,9)` instead of `(4,9)`. If we used
`None` instead that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```reasonligo group=maps
let add = (m : register) : register =>
  Map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);
```

</Syntax>
<Syntax syntax="jsligo">

We can update a binding in a map in JsLIGO by means of the
`Map.update` built-in function:

```jsligo group=maps
let assign = (m: register): register =>
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, Some ([4, 9]), m);
```

Notice the optional value `Some ([4,9])` instead of `[4, 9]`. If we used
`None` instead that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```jsligo group=maps
let add = (m: register): register =>
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, [4, 9], m);
```

</Syntax>

To remove a binding from a map, we need its key.



<Syntax syntax="pascaligo">

In PascaLIGO, there is a special instruction to remove a binding from
a map.
```pascaligo group=maps
function delete (const key : address; var moves : register) : register is
  block {
    remove key from map moves
  } with moves
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, we use the predefined function `Map.remove` as follows:

```cameligo group=maps
let delete (key, moves : address * register) : register =
  Map.remove key moves
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, we use the predefined function `Map.remove` as follows:

```reasonligo group=maps
let delete = ((key, moves) : (address, register)) : register =>
  Map.remove (key, moves);
```

</Syntax>
<Syntax syntax="jsligo">

In JsLIGO, we use the predefined function `Map.remove` as follows:

```jsligo group=maps
let delete = ([key, moves]: [address, register]): register =>
  Map.remove(key, moves);
```

</Syntax>



### Functional Iteration over Maps

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO maps: the
*iterated operation*, the *map operation* (not to be confused with the
*map data structure*) and the *fold operation*.

#### Iterated Operation over Maps

The first, the *iterated operation*, is an iteration over the map with
no return value: its only use is to produce side-effects. This can be
useful if, for example you would like to check that each value inside
of a map is within a certain range and fail with an error otherwise.

The predefined functional iterator implementing the iterated operation
over maps is called `Map.iter`. In the following example, the register
of moves is iterated to check that the start of each move is above
`3`.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function iter_op (const m : register) : unit is
  block {
    function iterated (const i : address; const j : move) : unit is
      if j.1 > 3 then Unit else (failwith ("Below range.") : unit)
  } with Map.iter (iterated, m)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let iter_op (m : register) : unit =
  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)
  in Map.iter predicate m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let iter_op = (m : register) : unit => {
  let predicate = ((i,j) : (address, move)) => assert (j[0] > 3);
  Map.iter (predicate, m);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let iter_op = (m: register): unit => {
  let predicate = ([i, j]: [address, move]): unit => assert(j[0] > 3);
  Map.iter(predicate, m);
};
```

</Syntax>


#### Map Operations over Maps

We may want to change all the bindings of a map by applying to them a
function. This is called a *map operation*, not to be confused with
the map data structure. The predefined functional iterator
implementing the map operation over maps is called `Map.map`. In the
following example, we add `1` to the ordinate of the moves in the
register.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function map_op (const m : register) : register is
  block {
    function increment (const i : address; const j : move) : move is
      (j.0, j.1 + 1)
  } with Map.map (increment, m)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let map_op (m : register) : register =
  let increment = fun (i,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let map_op = (m : register) : register => {
  let increment = ((i,j): (address, move)) => (j[0], j[1] + 1);
  Map.map (increment, m);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let map_op = (m: register): register => {
  let increment = ([i, j]: [address, move]): [int, int] => [j[0], j[1] + 1];
  return Map.map(increment, m);
};
```

</Syntax>


#### Folded Operations over Maps

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.

The predefined functional iterator implementing the folded operation
over maps is called `Map.fold` and is used as follows.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function fold_op (const m : register) : int is
  block {
    function folded (const i : int; const j : address * move) : int is
      i + j.1.1
  } with Map.fold (folded, m, 5)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let fold_op (m : register) : int =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let fold_op = (m : register) : int => {
  let folded = ((i,j): (int, (address, move))) => i + j[1][1];
  Map.fold (folded, m, 5);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let fold_op = (m: register): int => {
  let folded = ([i, j]: [int, [address, move]]): int => i + j[1][1];
  return Map.fold(folded, m, 5);
};
```

</Syntax>


## Big Maps

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary maps.

### Declaring a Map

Here is how we define a big map:


<Syntax syntax="pascaligo">

```pascaligo group=big_maps
type move is int * int
type register is big_map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
type move = int * int
type register = (address, move) big_map
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
type move = (int, int);
type register = big_map (address, move);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_maps
type move = [int, int];
type register = big_map<address, move>;
```

</Syntax>


### Creating an Empty Big Map

Here is how to create an empty big map.




<Syntax syntax="pascaligo">

```pascaligo group=big_maps
const empty : register = big_map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let empty : register = Big_map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let empty : register = Big_map.empty
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_maps
let empty: register = Big_map.empty;
```

</Syntax>

### Creating a Non-empty Map

And here is how to create a non-empty map value:


<Syntax syntax="pascaligo">

```pascaligo group=big_maps
const moves : register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

Notice the right arrow `->` between the key and its value and the
semicolon separating individual map entries. The value annotation
`("<string value>" : address)` means that we cast a string into an
address. -->

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

The predefined function `Big_map.literal` constructs a big map from a
list of key-value pairs `(<key>, <value>)`. Note also the semicolon
separating individual map entries.  The annotated value `("<string>
value>" : address)` means that we cast a string into an address.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let moves : register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

The predefined function `Big_map.literal` constructs a big map from a
list of key-value pairs `(<key>, <value>)`. Note also the semicolon
separating individual map entries.  The annotated value `("<string>
value>" : address)` means that we cast a string into an address.

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_maps
let moves : register =
  Big_map.literal (list([
    ["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address, [1, 2]],
    ["tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, [0, 3]]]));
```

The predefined function `Big_map.literal` constructs a big map from a
list of key-value pairs `[<key>, <value>]`. Note also the semicolon
separating individual map entries.  The annotated value `("<string>
value>" as address)` means that we cast a string into an address.

</Syntax>


### Accessing Values

If we want to access a move from our `register` above, we can use the
postfix `[]` operator to read the associated `move` value. However,
the value we read is an optional value (in our case, of type `option
(move)`), to account for a missing key. Here is an example:



<Syntax syntax="pascaligo">

```pascaligo group=big_maps
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_maps
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_maps
let my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_maps
let my_balance: option<move> =
  Big_map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, moves);
```

</Syntax>


### Updating Big Maps



<Syntax syntax="pascaligo">

The values of a PascaLIGO big map can be updated using the
assignment syntax for ordinary maps

```pascaligo group=big_maps
function assign (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=big_maps
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

</Syntax>
<Syntax syntax="cameligo">

We can update a big map in CameLIGO using the `Big_map.update`
built-in:

```cameligo group=big_maps
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

</Syntax>
<Syntax syntax="reasonligo">

We can update a big map in ReasonLIGO using the `Big_map.update`
built-in:

```reasonligo group=big_maps
let updated_map : register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```

</Syntax>
<Syntax syntax="jsligo">

We can update a big map in JsLIGO using the `Big_map.update`
built-in:

```jsligo group=big_maps
let updated_map: register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, Some([4, 9]), moves);
```

</Syntax>


### Removing Bindings

Removing a binding in a map is done differently according to the LIGO
syntax.



<Syntax syntax="pascaligo">

PascaLIGO features a special syntactic construct to remove bindings
from maps, of the form `remove <key> from map <map>`. For example,

```pascaligo group=big_maps
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the predefined function which removes a binding in a map
is called `Map.remove` and is used as follows:

```cameligo group=big_maps
let updated_map : register =
  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the predefined function which removes a binding in a map
is called `Map.remove` and is used as follows:

```reasonligo group=big_maps
let updated_map : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

</Syntax>
<Syntax syntax="jsligo">

In JsLIGO, the predefined function which removes a binding in a map
is called `Map.remove` and is used as follows:

```jsligo group=big_maps
let updated_map_: register =
  Map.remove("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, moves);
```

</Syntax>
