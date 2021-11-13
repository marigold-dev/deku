---
id: hangzhou
title: Hangzhou
description: Hangzhou changes 
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> Important: This is only available for protocol Hangzhou (i.e. `--protocol hangzhou` command line option).

> Note: this is a temporary documentation page and will be moved to another location.

## API

### New types

<SyntaxTitle syntax="pascaligo">
type chest
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type chest
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type chest
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type chest
</SyntaxTitle>
A type for chests

<SyntaxTitle syntax="pascaligo">
type chest_key
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type chest_key
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type chest_key
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type chest_key
</SyntaxTitle>
A type for chest keys

<SyntaxTitle syntax="pascaligo">
type chest_opening_result is
    Ok_opening of bytes
  | Fail_decrypt
  | Fail_timelock
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type chest_opening_result =
    Ok_opening of bytes
  | Fail_decrypt
  | Fail_timelock
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type chest_opening_result =
    Ok_opening(bytes)
  | Fail_decrypt
  | Fail_timelock
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type chest_opening_result =
   ["Ok_opening", bytes]
 | ["Fail_decrypt"]
 | ["Fail_timelock"];
</SyntaxTitle>
A type for the result of chest opening, see `Tezos.open_chest`

### New primitives

#### Tezos

<SyntaxTitle syntax="pascaligo">
function open_chest: chest_key -> chest -> nat -> chest_opening_result
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val open_chest : chest_key -> chest -> nat -> chest_opening_result
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let open_chest : chest_key => chest => nat => chest_opening_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let open_chest : chest_key => chest => nat => chest_opening_result
</SyntaxTitle>

<SyntaxTitle syntax="pascaligo">
function call_view: string -> 'arg -> address -> option('ret)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val call_view : string -> 'arg -> address -> 'ret option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let call_view : string => 'arg => address => option ('ret)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let call_view : string => 'arg => address => option &lt;&apos;ret&gt;
</SyntaxTitle>

#### Test

new signature for originate_from_file:

<SyntaxTitle syntax="pascaligo">
function originate_from_file : string -> string -> list(string) -> michelson_program -> tez -> (address * michelson_program * int)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val originate_from_file : string -> string -> string list -> michelson_program -> tez -> (address * michelson_program * int)
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let originate_from_file : string => string => list(string) => michelson_program => tez => (address, michelson_program, int)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate_from_file = (filepath: string, entrypoint: string , views : list &lt;&apos;string&gt; , init: michelson_program, balance: tez) => [address, michelson_program, int]
</SyntaxTitle>

Originate a contract with a path to the contract file, an entrypoint, a list of views, an initial storage and an initial balance.

<SyntaxTitle syntax="pascaligo">
function create_chest : bytes -> nat -> chest * chest_key
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val create_chest : bytes -> nat -> chest * chest_key
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let create_chest : bytes => nat => (chest , chest_key)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create_chest : bytes => nat => [chest , chest_key]
</SyntaxTitle>

Generate a locked value, the RSA parameters and encrypt the payload. Also returns the chest key  
Exposes tezos timelock library function [create_chest_and_chest_key](https://gitlab.com/tezos/tezos/-/blob/v11-release/src/lib_crypto/timelock.mli#L197)

<SyntaxTitle syntax="pascaligo">
function create_chest_key : chest -> nat -> chest_key
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val create_chest_key : chest -> nat -> chest_key
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let create_chest_key : chest => nat => chest_key
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create_chest_key : chest => nat => chest_key
</SyntaxTitle>

Unlock the value and create the time-lock proof.  
Exposes tezos timelock library function [create_chest_key](https://gitlab.com/tezos/tezos/-/blob/v11-release/src/lib_crypto/timelock.mli#L201).

## Examples

### Timelock

Extensive documentation about timelock can be found [here](https://tezos.gitlab.io/alpha/timelock.html#timelock).
Here is an example of a contract trying to open a chest and the corresponding tests to trigger all error kinds:

<Syntax test-ligo syntax="cameligo">

```cameligo test-ligo group=timelock protocol=hangzhou
type storage = bytes
type parameter = chest_key * chest

type return = operation list * storage

let main (p , _ : parameter * storage) : return =
  let (ck,c) = p in
  let new_s =
    match Tezos.open_chest ck c 10n with
    | Ok_opening b -> b
    | Fail_timelock -> 0x00
    | Fail_decrypt -> 0x01
  in
  (([] : operation list), new_s)


let test =
  let init_storage : bytes = 0x00 in
  let (addr,_,_) = Test.originate main init_storage 0tez in
  let payload = 0x0101 in

  let test_open (cc : chest_key * chest) (expected : bytes) : unit =
    let x : parameter contract = Test.to_contract addr in
    let () = Test.transfer_to_contract_exn x cc 0tez in
    let s = Test.get_storage addr in
    assert (s = expected)
  in

  let test1 = (* chest key/payload and time matches -> OK *)
    let (chest,chest_key) = Test.create_chest payload 10n in
    test_open (chest_key,chest) payload
  in
  let test2 = (* chest key/payload do not match -> Fail_decrypt *)
    let (chest,_) = Test.create_chest payload 10n in
    let (_,chest_key) = Test.create_chest 0x2020 10n in
    test_open (chest_key,chest) 0x01
  in
  let test3 = (* chest time do not match -> Fail_timelock *)
    let (chest,_) = Test.create_chest payload 2n in
    let chest_key = Test.create_chest_key chest 10n in
    test_open (chest_key,chest) 0x00
  in
  ()

```

<!-- TODO TRANSLATE THE CONTRACT ABOVE :) and add test-ligo in the code block arg -->
</Syntax>
<Syntax syntax="jsligo">

```jsligo group=timelock protocol=hangzhou
let open_or_fail = ([ck , c , time_] : [chest_key, chest, nat]) : bytes => {
  return (match ( Tezos.open_chest(ck,c,time_), {
    Ok_opening: (b:bytes) => b,
    Fail_decrypt: () => failwith("decrypt") as bytes,
    Fail_timelock: () => failwith("timelock") as bytes,
  }))
};
```

</Syntax>
<Syntax syntax="religo">

```religo group=timelock protocol=hangzhou
let open_or_fail = ((ck , c , time_) : (chest_key, chest, nat)) : bytes => {
  switch (Tezos.open_chest(ck,c,time_)) {
    | Ok_opening b => b
    | Fail_decrypt => (failwith("decrypt") : bytes)
    | Fail_timelock => (failwith("timelock") : bytes)
  }
};
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo group=timelock protocol=hangzhou
function open_or_fail (const ck : chest_key ; const c : chest ; const time_ : nat) : bytes is
  case (Tezos.open_chest(ck,c,time_)) of
    | Ok_opening (b) -> b
    | Fail_decrypt -> (failwith("decrypt") : bytes)
    | Fail_timelock -> (failwith("timelock") : bytes)
  end
```

</Syntax>


### On-chain views

> Tezos documentation on views can be found [here](https://tezos.gitlab.io/011/michelson.html#operations-on-views)

On-chain views are named routines attached to your contract allowing another contract to call them to
get a "view" of your contract current storage. It cannot modify your storage nor emit operations.  
These routines can either simply return your contract storage or apply some kind of processing to it:
they take your current storage, a parameter and returns the data of your choice. Note that parameter and return types can be anything except `big_map` ; `sapling_state` ; `operation` and `ticket`.  
Views are named after their declaration name and can be compiled in two ways:

1. by passing their names to the command line option `--views` (e.g. `ligo compile contract --views v1,v2,v3`)
2. by annotating their declarations in your code with `view`

> Important: the first way (`--views`) will override any annotated declarations

Given a very simple contract having a storage of type `string`, here are a few legit views:

<Syntax syntax="cameligo">

```cameligo group=views protocol=hangzhou
type storage = string
let main (((),s): unit * storage) : operation list * storage = ([]:operation list) , s

(* view 'view1', simply returns the storage *)
[@view] let view1 ((),s: unit * storage) : storage = s

(* view 'v2', returns true if the storage has a given length *)
[@view] let v2 (expected_length,s: nat * storage) : bool = (String.length s = expected_length)

(* view 'v3' returns a constant int *)
[@view] let v3 ((),_ : unit * storage) : int = 42
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=views protocol=hangzhou
type storage = string
let main = ([_ , s]: [unit , storage]) : [ list<operation> , storage] => [list([]) as list<operation> , s];

/* view 'view1', simply returns the storage */
// @view
let view1 = ([_ , s]: [unit , storage]) : storage => s;

/* view 'v2', returns true if the storage has a given length */
// @view
let v2 = ([expected_length,s] : [nat , storage]) : bool => (String.length (s) == expected_length);

/* view 'view3' returns a constant int */
// @view
let view3 = ([_ , _s]: [unit , storage]) : int => 42;
```

</Syntax>
<Syntax syntax="religo">

```religo group=views protocol=hangzhou
type storage = string
let main = ((_ , s): (unit , storage)) : (list(operation) , storage) => (([] : list(operation)) , s);

/* view 'view1', simply returns the storage */
[@view] let view1 = ((_ , s): (unit , storage)) : storage => s;

/* view 'v2', returns true if the storage has a given length */
[@view] let v2 = ((expected_length,s) : (nat , storage)) : bool => (String.length (s) == expected_length);

/* view 'view3' returns a constant int */
[@view] let view3 = ((_ , _): (unit , storage)) : int => 42;
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo group=views protocol=hangzhou
type storage is string
function main (const _ : unit ; const s : storage) : list (operation) * storage is ((nil : list(operation)) , s)

(* view 'view1', simply returns the storage *)
[@view] function view1 (const _ : unit ; const s: storage) : storage is s

(* view 'v2', returns true if the storage has a given length *)
[@view] function v2 (const expected_length : nat ; const s: storage) : bool is (String.length (s) = expected_length)

(* view 'v3' returns a constant int *)
[@view] function v3 (const _ : unit ; const _ : storage) : int is 42
```

</Syntax>

A few primitives have a slightly different meaning when executed as part of a view:

- `Tezos.balance` represents the current amount of mutez held by the contract attached to the view
- `Tezos.sender` represents the caller of the view
- `Tezos.amount` is always 0 mutez
- `Tezos.self_address` represents the contract attached to the view

On the caller side, the primitive `Tezos.call_view` will allow you to call another contract view and get its result by providing the view name; the contract address and the parameter of the view. If the address is nonexistent; the name does not match of of the contract
view or the parameter type do not match, `Tezos.call_view` will return `None`.

<Syntax syntax="cameligo">

```cameligo group=views protocol=hangzhou
let view_call ((name,parameter,addr): string * int * address) : int option = Tezos.call_view "sto_plus_n" 1 addr
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=views protocol=hangzhou
let view_call = ([name,parameter,addr]: [string , int , address]) : option<int> => (Tezos.call_view ("sto_plus_n", 1, addr) as option<int>)
```

</Syntax>
<Syntax syntax="religo">

```religo group=views protocol=hangzhou
let view_call = ((name,parameter,addr): (string , int , address)) : option(int) => Tezos.call_view ("sto_plus_n", 1, addr)
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo group=views protocol=hangzhou
function view_call (const name : string ; const parameter : int ; const addr: address) : option(int) is Tezos.call_view ("sto_plus_n", 1, addr)
```

</Syntax>
