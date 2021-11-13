---
id: map-reference
title: Map
description: Map operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
function empty : map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val empty : ('key, 'value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let empty: map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let empty: map&lt;'key, 'value&gt;
</SyntaxTitle>

Create an empty map.

<Syntax syntax="pascaligo">

```pascaligo group=maps
type move is int * int
type register is map (address, move)

const empty : register = Map.empty
```

Or

```pascaligo group=maps
const empty : register = map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
type move = int * int
type register = (address, move) map

let empty : register = Map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
type move = (int, int);
type register = map (address, move);

let empty : register = Map.empty
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
type move = [int, int];
type register = map<address, move>;

let empty: register = Map.empty;
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function literal : list ('key * 'value) -> map ('key, 'value) 
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val literal : ('key * 'value) list -> ('key, 'value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let literal: list(('key, 'value)) => map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let literal: (input: list&lt;['key, 'value]&gt;) => map&lt;'key, 'value&gt;
</SyntaxTitle>

Create a non-empty map.

<Syntax syntax="pascaligo">

```pascaligo group=maps
const moves : register =
  Map.literal (list [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]);
```

Alternative way of creating an empty map:

```pascaligo group=maps
const moves_alternative : register =
  map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)];
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let moves : register =
  Map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let moves : register =
  Map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let moves: register =
  Map.literal(list([
    [("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address), [1, 2]],
    [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [0, 3]]]));
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function find_opt : 'key -> map ('key, 'value) -> option 'value
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val find_opt : 'key -> ('key, 'value) map -> 'value option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let find_opt : ('key, map ('key, 'value)) => option('value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let find_opt : (key: 'key, map: map &lt;'key, 'value&gt;) => option &lt;'value&gt;
</SyntaxTitle>

Retrieve a (option) value from a map with the given key. Returns `None` if the 
key is missing and the value otherwise.


<Syntax syntax="pascaligo">

```pascaligo group=maps
const my_balance : option (move) =
  Map.find_opt (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves)
```

Alternatively:

```pascaligo group=maps
const my_balance_alternative : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)];
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
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let my_balance: option<move> =
  Map.find_opt(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function update : 'key -> option 'value -> map ('key, 'value) -> map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val update: 'key -> 'value option -> ('key, 'value) map -> ('key, 'value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let update: ('key, option('value), map('key, 'value)) => map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update: (key: 'key, new_value: option&lt;'value&gt;, map: map&lt;'key, 'value&gt;) => map &lt;'key, 'value&gt;
</SyntaxTitle>

Note: when `None` is used as a value, the key and associated value is removed 
from the map.

<Syntax syntax="pascaligo">

```pascaligo group=maps
  const updated_map : register = Map.update(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some (4,9), moves);
```

Alternatively:

```pascaligo group=maps

function update (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9);
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

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let updated_map : register =
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let updated_map : register =
  Map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let updated_map : register =
  Map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), Some ([4, 9]), moves);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function get_and_update : key -> option(value) -> map (key, value) -> option(value) * map (key, value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_and_update : 'key -> 'value option -> ('key, 'value) map -> 'value option * ('key, 'value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_and_update : ('key, option('value), map('key, 'value)) => (option('value), map ('key, 'value))
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_and_update : (key : 'key, value : option&lt;'value&gt;, map : map&lt;'key, 'value&gt;) => [option&lt;'value&gt;, map&lt;'key, 'value&gt;]
</SyntaxTitle>

Similar to `update` but it also returns the value that was previously stored in the map

<Syntax syntax="pascaligo">

```pascaligo group=maps
const updated : option(move) * register = 
  Map.get_and_update (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (Some (4, 9)), moves);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let (old_move_opt, updated_map) : (move option * register) =
  Map.get_and_update ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4, 9)) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let (old_move_opt, updated_map) : (option(move), register) = 
  Map.get_and_update (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (Some (4, 9)), moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let [old_move, updated_map2] : [option<move>, register] = 
  Map.get_and_update (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), (Some([24, 48] as move)), moves);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function add : 'key -> 'value -> map ('key, 'value) -> map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val add : 'key -> 'value -> ('key, 'value) map  -> ('key, 'value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let add: ('key, 'value, map('key, 'value)) => map('key, 'value) 
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add: (key: 'key, value: 'value, map: map&lt;'key, 'value&gt;) => map&lt;'key, 'value&gt;
</SyntaxTitle>

Returns a new map with key-value pair added to the input map

<Syntax syntax="pascaligo">

```pascaligo group=maps
const added_item : register = Map.add (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4, 9), moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let add (m : register) : register =
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let add = (m: register): register =>
  Map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let add = (m: register): register =>
  Map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [4, 9], m);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function remove : 'key -> map ('key, 'value) -> map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val remove : 'key -> ('key, 'value) map -> ('key, 'value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let remove: ('key, map('key, 'value)) => map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let remove: ('key: key, map: map&lt;'key, 'value&gt;) => map&lt;'key, 'value&gt;
</SyntaxTitle>

Returns a new map with key-value pair removed from the input map

<Syntax syntax="pascaligo">

```pascaligo group=maps
  const updated_map : register = 
    Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

Alternatively, the instruction `remove key from map m` removes the key
`key` from the map `m`.

```pascaligo group=maps
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let updated_map : register =
  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let updated_map : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let updated_map3 : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function iter : ((key, value) -> unit) -> map (key, value) -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val iter : (('key * 'value) -> unit) -> ('key, 'value) map -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let iter: ((('key, 'value)) => unit, map('key, 'value)) => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let iter: (iter: (['key, 'value]) => unit, map: map&lt;'key, 'value&gt;) => unit
</SyntaxTitle>

Iterate over key-value pairs in a map

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
let iter_op = (m : register) : unit => {
  let predicate = ([i, j] : [address, move]): unit => assert (j[0] > 3);
  Map.iter (predicate, m);
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function map : (('key, 'value) -> 'mapped_value) -> map ('key, 'value) -> map ('key, 'mapped_value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val map : (('key * 'value) -> 'mapped_value) -> ('key, 'value) map -> ('key, 'mapped_value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let map: ((('key, 'value)) => 'mapped_value, map('key, 'value)) => map('key, 'mapped_value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let map: (mapper: (item: ['key, 'value]) => 'mapped_value, map: map&lt;'key, 'value&gt;) => map&lt;'key, 'mapped_value&gt;
</SyntaxTitle>

Applies the mapper function on the key-value pairs of map and builds a new map

<Syntax syntax="pascaligo">

```pascaligo group=maps
function map_op (const m : register) : register is
  block {
    function increment (const _i : address; const j : move) : move is
      (j.0, j.1 + 1)
  } with Map.map (increment, m)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let map_op (m : register) : register =
  let increment = fun (_i,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let map_op = (m : register) : register => {
  let increment = ((_i,j): (address, move)) : move => (j[0], j[1] + 1);
  Map.map (increment, m);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let map_op = (m : register) : register => {
  let increment = ([_i,j] : [address, move]) : move => [j[0], j[1] + 1];
  return Map.map (increment, m);
};
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function fold : (('accumulator -> ('key, 'value) -> 'accumulator) -> map ('key, 'value) -> 'accumulator) -> 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold : (('accumulator * ('key * 'value)) -> 'accumulator) -> ('key, 'value) map -> 'accumulator -> 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let fold: ((('accumulator, ('key, 'value)) => 'accumulator), map('key, 'value), 'accumulator) => 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let fold: (iter: ((accumulator: 'accumulator, item: ['key, 'value]) => 'accumulator), map: map&lt;'key, 'value&gt;, accumulator: 'accumulator) => 'accumulator
</SyntaxTitle>

Fold over key-value pairs of a map

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
let fold_op = (m : register): int => {
  let folded = ([i,j]: [int, [address, move]]):int => i + j[1][1];
  return Map.fold (folded, m, 5);
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function size : map ('key, 'value) -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val size : ('key, 'value) map -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let size: map('key, 'value) => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let size: (map: map&lt;'key, 'value&gt;) => nat
</SyntaxTitle>

Returns the number of items in the map.

<Syntax syntax="pascaligo">

```pascaligo group=maps
const size_ : nat = Map.size (moves);
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=maps
let size_ : nat = Map.size  moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let size_ : nat = Map.size (moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let size_ : nat = Map.size(moves);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function mem : 'key -> map ('key, 'value) -> bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val mem : 'key -> ('key, 'value) map -> bool
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let mem : ('key, map('key, 'value)) => bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mem : (key: 'key, map: map&lt;'key, 'value&gt;) => bool
</SyntaxTitle>

Checks if a key exists in the map.

<Syntax syntax="pascaligo">

```pascaligo group=maps
const found : bool = Map.mem (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), moves);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let found : bool = Map.mem ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)  moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let found : bool = Map.mem (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=maps
let found : bool = Map.mem (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address),  moves);
```

</Syntax>
