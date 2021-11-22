---
id: big-map-reference
title: Big_map
description: A lazily deserialised map that's intended to store large amounts of data.
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

A lazily deserialised map that's intended to store large amounts of data. 
"Lazily" means that storage is read or written per key on demand. Therefore 
there are no `map`, `fold`, and `iter` operations as there are in 
[Map](map.md).

Compared to non-lazy maps, which have a high upfront gas cost to deserialise all
the data and then have cheaper access costs thereafter, lazily-deserialised maps
spread this cost out across each access, increasing the per-access gas costs,
but providing a cheaper overall cost when only a small portion of a large 
data-set is needed.

<SyntaxTitle syntax="pascaligo">
function empty : big_map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val empty : ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let empty: big_map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let empty: big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

Create an empty big_map.

<Syntax syntax="pascaligo">

```pascaligo group=big_map
type move is int * int
type register is big_map (address, move)

const empty : register = Big_map.empty
```

Alternatively, you can also create an empty big_map using:

```pascaligo group=big_map
const empty_alternative : register = big_map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
type move = int * int
type register = (address, move) big_map

let empty : register = Big_map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
type move = (int, int);
type register = big_map(address, move);

let empty: register = Big_map.empty
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_map
type move = [int, int];
type register = big_map<address, move>;

let empty: register = Big_map.empty;
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function literal : list ('key * 'value) -> big_map ('key, 'value) 
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val literal : ('key * 'value) list -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let literal: list(('key, 'value)) => big_map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let literal: (items: list&lt;[&apos;key, &apos;value]&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

Create a non-empty big_map.

<Syntax syntax="pascaligo">

```pascaligo group=big_map
const moves : register =
  Big_map.literal (list [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]);
```

Alternative way of creating an empty big_map:

```pascaligo group=big_map
const moves_alternative : register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)];
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let moves: register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_map
let moves: register =
  Big_map.literal (list([
    [("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address), [1, 2]],
    [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [0, 3]]]));
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function find_opt : 'key -> big_map ('key, 'value) -> option 'value
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val find_opt : 'key -> ('key, 'value) big_map -> 'value option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let find_opt: ('key, big_map ('key, 'value)) => option ('value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let find_opt: (key: &apos;key, big_map: big_map &lt;&apos;key, &apos;value&gt;) => option &lt;&apos;value&gt;
</SyntaxTitle>

Retrieve a value from a big map with the given key. 

Because the key may be missing in the big map, the result is an 
*optional value*.


<Syntax syntax="pascaligo">

```pascaligo group=big_map
const my_balance : option (move) =
  Big_map.find_opt (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves)
```

Alternatively:

```pascaligo group=big_map
const my_balance_alternative : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)];
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let my_balance: option (move) =
  Big_map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_map
let my_balance: option <move> =
  Big_map.find_opt(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function mem : 'key -> big_map ('key, 'value) -> bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val mem : 'key -> ('key, 'value) big_map -> bool
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let mem: ('key, big_map ('key, 'value)) => bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mem: (key: &apos;key, big_map: big_map &lt;&apos;key, &apos;value&gt;) => bool
</SyntaxTitle>

Test whether a given key exists within a big map. 

<Syntax syntax="pascaligo">

```pascaligo group=big_map
const has_balance : bool =
  Big_map.mem (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let has_balance : bool =
  Big_map.mem ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let has_balance: bool =
  Big_map.mem("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_map
let has_balance: bool =
  Big_map.mem(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function update : 'key -> option 'value -> big_map ('key, 'value) -> big_map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val update: 'key -> 'value option -> ('key, 'value) big_map -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let update: ('key, option('value), big_map('key, 'value)) => big_map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update: (key: &apos;key, value: option&lt;&apos;value&gt;, big_map: big_map&lt;&apos;key, &apos;value&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

Note: when `None` is used as a value, the value is removed from the big_map.

<Syntax syntax="pascaligo">

```pascaligo group=big_map
  const updated_big_map : register = Big_map.update(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some (4,9), moves);
```

Alternatively:

```pascaligo group=big_map

function update (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9);
  } with m
  
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=big_map
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

> Note the use of the keyword `map` instead of `big_map` (which is not
> a keyword).

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let updated_map: register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some((4,9)), moves);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_map
let updated_map: register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), Some([4,9]), moves);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function get_and_update : key -> option(value) -> big_map (key, value) -> option(value) * big_map (key, value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_and_update : 'key -> 'value option -> ('key, 'value) big_map -> value option * ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_and_update : 'key => option('value) => big_map ('key, 'value) => option('value) * big_map ('key, 'value)
</SyntaxTitle>

Similar to `update` but it also returns the value that was previously stored in the big_map

<SyntaxTitle syntax="pascaligo">
function add : 'key -> 'value -> big_map ('key, 'value) -> big_map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val add : 'key -> 'value -> ('key, 'value) big_map  -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let add: ('key, 'value, big_map('key, 'value)) => big_map('key, 'value) 
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add: (key: &apos;key, value: &apos;value, big_map: big_map&lt;&apos;key, &apos;value&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>
<Syntax syntax="pascaligo">

```pascaligo group=big_map
const added_item : register = Big_map.add (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4, 9), moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let add (m : register) : register =
  Big_map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let add = (m: register): register =>
  Big_map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), (4,9), m);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_map
let add = (m: register): register =>
  Big_map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [4,9], m);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function remove: 'key -> big_map ('key, 'value) -> big_map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val remove: 'key -> ('key, 'value) big_map -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let remove: ('key, big_map('key, 'value)) => big_map('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let remove: (key: &apos;key, big_map: big_map&lt;&apos;key, &apos;value&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

<Syntax syntax="pascaligo">

```pascaligo group=big_map
  const updated_map : register = 
    Big_map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

Alternatively, the instruction `remove key from map m` removes the key
`key` from the big map `m` (note that the keyword is `map`, not
`big_map`).

```pascaligo group=big_map
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let updated_map : register =
  Big_map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let updated_map: register =
  Big_map.remove(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=big_map
let updated_map_: register =
  Big_map.remove(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves); 
```

</Syntax>

