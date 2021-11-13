---
id: tezos-taco-shop-smart-contract
title: The Taco Shop Smart Contract
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

<div>

Meet **Pedro**, our *artisan taco chef*, who has decided to open a
Taco shop on the Tezos blockchain, using a smart contract. He sells
two different kinds of tacos: **el Clásico** and the **Especial
del Chef**.

To help Pedro open his dream taco shop, we will implement a smart
contract that will manage supply, pricing & sales of his tacos to the
consumers.

<br/>
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/taco-stand.svg" width="50%" />
<div style={{ opacity: 0.7, textAlign: 'center', fontSize: '10px' }}>Made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/" 			    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" 			    title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>

---

## Pricing

Pedro's tacos are a rare delicacy, so their **price goes up** as the
**stock for the day begins to deplete**.

Each taco kind, has its own `max_price` that it sells for, and a
finite supply for the current sales life-cycle.

> For the sake of simplicity, we will not implement the replenishing
> of the supply after it has run out.

### Daily Offer

|**kind** |id |**available_stock**| **max_price**|
|---|---|---|---|
|Clásico | `1n` | `50n` | `50tez` |
|Especial del Chef | `2n` | `20n` | `75tez` |

### Calculating the Current Purchase Price

The current purchase price is calculated with the following formula:

```pascaligo skip
current_purchase_price = max_price / available_stock
```

#### El Clásico
|**available_stock**|**max_price**|**current_purchase_price**|
|---|---|---|
| `50n` | `50tez` | `1tez`|
| `20n` | `50tez` | `2.5tez` |
| `5n` | `50tez` | `10tez` |

#### Especial del chef
|**available_stock**|**max_price**|**current_purchase_price**|
|---|---|---|
| `20n` | `75tez` | `3.75tez` |
| `10n` | `75tez` | `7.5tez`|
| `5n` | `75tez` | `15tez` |

---

## Installing LIGO

In this tutorial, we will use LIGO's dockerised version, for the sake
of simplicity. You can find the installation instructions
[here](../../intro/installation.md#dockerized-installation-recommended).

## Implementing our First `main` Function

> From now on we will get a bit more technical. If you run into
> something we have not covered yet - please try checking out the
> [LIGO cheat sheet](api/cheat-sheet.md) for some extra tips & tricks.

To begin implementing our smart contract, we need a *main function*,
that is the first function being executed. We will call it `main` and
it will specify our contract's storage (`int`) and input parameter
(`int`). Of course this is not the final storage/parameter of our
contract, but it is something to get us started and test our LIGO
installation as well.

<Syntax syntax="pascaligo">

```pascaligo group=a
(* taco-shop.ligo *)
function main (const parameter : int; const contractStorage : int) : list (operation) * int is
  ((nil : list (operation)), contractStorage + parameter)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=a
let main = ((parameter, contractStorage) : (int, int)) : (list (operation), int) => {
  (([] : list (operation)), contractStorage + parameter)
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=a
let main = ([parameter, contractStorage] : [int, int]) : [list <operation>, int] => {
  return [
    (list([]) as list <operation>), contractStorage + parameter
  ]
};
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let main (parameter, contractStorage : int * int) : operation list * int =
  ([] : operation list), contractStorage + parameter
```

</Syntax>
Let us break down the contract above to make sure we understand each
bit of the LIGO syntax:

- **`function main`** - definition of the main function, which takes
  the parameter of the contract and the storage
- **`(const parameter : int;  const contractStorage : int)`** -
  parameters passed to the function: the first is called `parameter`
  because it denotes the parameter of a specific invocation of the
  contract, the second is the storage
- **`(list (operation) * int)`** - return type of our function, in our
  case a tuple with a list of operations, and an `int` (new value for
  the storage after a successful run of the contract)
- **`((nil : list (operation)), contractStorage + parameter)`** -
  essentially a return statement
- **`(nil : list (operation))`**  - a `nil` value annotated as a list
  of operations, because that is required by our return type specified
  above
  - **`contractStorage + parameter`** - a new storage value for our
  contract, sum of previous storage and a transaction parameter

### Running LIGO for the First Time

To test that we have installed LIGO correctly, and that
`taco-shop.ligo` is a valid contract, we will dry-run it.

> Dry-running is a simulated execution of the smart contract, based on
> a mock storage value and a parameter. We will later see a better
> way to test contracts: The LIGO test framework

Our contract has a storage of `int` and accepts a parameter that is
also an `int`.

The `dry-run` command requires a few parameters:
- **contract** *(file path)*
- **entrypoint** *(name of the main function in the contract)*
- **parameter** *(parameter to execute our contract with)*
- **storage** *(starting storage before our contract's code is executed)*

It outputs what is returned from our main function: in our case a
tuple containing an empty list (of operations to apply) and the new
storage value, which, in our case, is the sum of the previous storage
and the parameter we have used for the invocation.

<Syntax syntax="pascaligo">

```zsh
ligo run dry-run taco-shop.ligo 4 3 --entry-point main
# OUTPUT:
# ( LIST_EMPTY() , 7 )
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo run dry-run taco-shop.mligo 4 3 --entry-point main
# OUTPUT:
# ( LIST_EMPTY() , 7 )
```

</Syntax>
<Syntax syntax="reasonligo">

```zsh
ligo run dry-run taco-shop.religo 4 3 --entry-point main
# OUTPUT:
# ( LIST_EMPTY() , 7 )
```

</Syntax>
<Syntax syntax="jsligo">

```zsh
ligo run dry-run taco-shop.jsligo 4 3 --entry-point main
# OUTPUT:
# ( LIST_EMPTY() , 7 )
```

</Syntax>

*`3 + 4 = 7` yay! Our CLI & contract work as expected, we can move onto fulfilling Pedro's on-chain dream.*

---

## Designing the Taco Shop's Contract Storage

We know that Pedro's Taco Shop serves two kinds of tacos, so we will
need to manage stock individually, per kind. Let us define a type,
that will keep the `stock` & `max_price` per kind in a record with two
fields. Additionally, we will want to combine our `taco_supply` type
into a map, consisting of the entire offer of Pedro's shop.

**Taco shop's storage**

<Syntax syntax="pascaligo">

```pascaligo group=b
type taco_supply is record [ current_stock : nat ; max_price : tez ]

type taco_shop_storage is map (nat, taco_supply)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
type taco_supply = { current_stock : nat ; max_price : tez }

type taco_shop_storage = (nat, taco_supply) map
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
type taco_supply = { current_stock : nat , max_price : tez } ;

type taco_shop_storage = map (nat, taco_supply) ;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
type taco_supply = { current_stock : nat , max_price : tez } ;

type taco_shop_storage = map <nat, taco_supply> ;
```

</Syntax>

Next step is to update the `main` function to include
`taco_shop_storage` in its storage. In the meanwhile, let us set the
`parameter` to `unit` as well to clear things up.

**`taco-shop.ligo`**

<Syntax syntax="pascaligo">

```pascaligo group=b

type return is list (operation) * taco_shop_storage

function main (const parameter : unit; const taco_shop_storage :  taco_shop_storage) : return is
  ((nil : list (operation)), taco_shop_storage)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
type return = operation list * taco_shop_storage

let main (parameter, taco_shop_storage : unit * taco_shop_storage) : return =
  (([] : operation list), taco_shop_storage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
type return = (list(operation), taco_shop_storage)

let main = ((parameter, taco_shop_storage) : (unit, taco_shop_storage)) : return => {
  (([] : list (operation)), taco_shop_storage)
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
type return_ = [list <operation>, taco_shop_storage];

let main = ([parameter, taco_shop_storage] : [unit, taco_shop_storage]) : return_ => {
  return [(list([]) as list <operation>), taco_shop_storage]
};
```

</Syntax>


### Populating our Storage

When deploying contract, it is crucial to provide a correct
initial storage value.  In our case the storage is type-checked as
`taco_shop_storage`. Reflecting
[Pedro's daily offer](tezos-taco-shop-smart-contract.md#daily-offer),
our storage's value will be defined as follows:

<Syntax syntax="pascaligo">

```pascaligo group=b
const init_storage : taco_shop_storage = map [
  1n -> record [ current_stock = 50n ; max_price = 50tez ] ;
  2n -> record [ current_stock = 20n ; max_price = 75tez ] ;
]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let init_storage : taco_shop_storage = Map.literal [
  (1n, { current_stock = 50n ; max_price = 50tez }) ;
  (2n, { current_stock = 20n ; max_price = 75tez }) ;
]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let init_storage : taco_shop_storage = Map.literal ([
  (1n, { current_stock : 50n , max_price : 50tez }),
  (2n, { current_stock : 20n , max_price : 75tez })
])
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let init_storage : taco_shop_storage = Map.literal (list([
  [1 as nat, { current_stock : 50 as nat, max_price : 50 as tez }],
  [2 as nat, { current_stock : 20 as nat, max_price : 75 as tez }]
]));
```

</Syntax>

> The storage value is a map with two bindings (entries) distinguished
> by their keys `1n` and `2n`.

Out of curiosity, let's try to use LIGO `compile-expression` command compile this value down to Michelson.

<Syntax syntax="pascaligo">

```zsh
ligo compile expression pascaligo --init-file gitlab-pages/docs/tutorials/get-started/pre_taco1.ligo init_storage
# Output:
#
# { Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo compile expression pascaligo --init-file gitlab-pages/docs/tutorials/get-started/pre_taco1.mligo init_storage
# Output:
#
# { Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }
```

</Syntax>
<Syntax syntax="reasonligo">

```zsh
ligo compile expression pascaligo --init-file gitlab-pages/docs/tutorials/get-started/pre_taco1.religo init_storage
# Output:
#
# { Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }
```

</Syntax>
<Syntax syntax="jsligo">

```zsh
ligo compile expression pascaligo --init-file gitlab-pages/docs/tutorials/get-started/pre_taco1.jsligo init_storage
# Output:
#
# { Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }
```

</Syntax>

Our initial storage record is compiled to a Michelson map `{ Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }`
holding the `current_stock` and `max_prize` in as a pair.

---

## Providing another Access Function for Buying Tacos

Now that we have our stock well defined in form of storage, we can
move on to the actual sales. The `main` function will take a key `id`
from our `taco_shop_storage` map and will be renamed `buy_taco` for
more readability. This will allow us to calculate pricing, and if the
sale is successful, we will be able to reduce our stock because we
have sold a taco!

### Selling the Tacos for Free

Let is start by customising our contract a bit, we will:

- rename `parameter` to `taco_kind_index`
- only in PascaLIGO syntax: change `taco_shop_storage` to a `var` instead of a `const`, because
  we will want to modify it

<Syntax syntax="pascaligo">

```pascaligo group=b
function buy_taco (const taco_kind_index : nat; var taco_shop_storage : taco_shop_storage) : return is
  ((nil : list (operation)), taco_shop_storage)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let buy_taco (taco_kind_index, taco_shop_storage : nat * taco_shop_storage) : return =
  (([] : operation list), taco_shop_storage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let buy_taco = ((taco_kind_index, taco_shop_storage) : (nat, taco_shop_storage)) : return => {
  (([] : list (operation)), taco_shop_storage)
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let buy_taco = ([taco_kind_index, taco_shop_storage] : [nat, taco_shop_storage]) : return_ => {
  return [(list([]) as list <operation>), taco_shop_storage]
};
```

</Syntax>


#### Decreasing `current_stock` when a Taco is Sold

In order to decrease the stock in our contract's storage for a
specific taco kind, a few things needs to happen:

- retrieve the `taco_kind` from our storage, based on the
  `taco_kind_index` provided;
- subtract the `taco_kind.current_stock` by `1n`;
- we can find the absolute value of the subtraction above by
  calling `abs` (otherwise we would be left with an `int`);
- update the storage, and return it.

<Syntax syntax="pascaligo">

```pascaligo group=b
function buy_taco (const taco_kind_index : nat; var taco_shop_storage : taco_shop_storage) : return is
  block {
    // Retrieve the taco_kind from the contract's storage or fail
    var taco_kind : taco_supply :=
      case taco_shop_storage[taco_kind_index] of
        Some (kind) -> kind
      | None -> (failwith ("Unknown kind of taco.") : taco_supply)
      end;

    // Decrease the stock by 1n, because we have just sold one
    taco_kind.current_stock := abs (taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind
  } with ((nil : list (operation)), taco_shop_storage)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let buy_taco (taco_kind_index, taco_shop_storage : nat * taco_shop_storage) : return =
  (* Retrieve the taco_kind from the contract's storage or fail *)
  let taco_kind : taco_supply =
    match Map.find_opt (taco_kind_index) taco_shop_storage with
    | Some k -> k
    | None -> (failwith "Unknown kind of taco" : taco_supply)
  in
  (* Update the storage decreasing the stock by 1n *)
  let taco_shop_storage = Map.update
    taco_kind_index
    (Some { taco_kind with current_stock = abs (taco_kind.current_stock - 1n) })
    taco_shop_storage
  in
  (([]: operation list), taco_shop_storage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let buy_taco = ((taco_kind_index, taco_shop_storage) : (nat, taco_shop_storage)) : return => {
  /* Retrieve the taco_kind from the contract's storage or fail */
  let taco_kind : taco_supply =
    switch (Map.find_opt (taco_kind_index, taco_shop_storage)) {
    | Some k => k
    | None => (failwith ("Unknown kind of taco"): taco_supply) } ;
  /* Update the storage decreasing the stock by 1n */
  let taco_shop_storage = Map.update (
    taco_kind_index,
    (Some ({...taco_kind, current_stock : abs (taco_kind.current_stock - 1n) })),
    taco_shop_storage );
  (([]: list(operation)), taco_shop_storage)
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let buy_taco2 = ([taco_kind_index, taco_shop_storage] : [nat, taco_shop_storage]) : return_ => {
  /* Retrieve the taco_kind from the contracts storage or fail */
  let taco_kind : taco_supply =
    match (Map.find_opt (taco_kind_index, taco_shop_storage), {
      Some: (k:taco_supply) => k,
      None: (_:unit) => (failwith ("Unknown kind of taco") as taco_supply)
    }) ;
  /* Update the storage decreasing the stock by 1n */
  let taco_shop_storage_ = Map.update (
    taco_kind_index,
    (Some (({...taco_kind, current_stock : abs (taco_kind.current_stock - (1 as nat)) }))),
    taco_shop_storage );
  return [(list([]) as list <operation>), taco_shop_storage_]
};
```

</Syntax>

### Making Sure We Get Paid for Our Tacos

In order to make Pedro's taco shop profitable, he needs to stop giving
away tacos for free. When a contract is invoked via a transaction, an
amount of tezzies to be sent can be specified as well. This amount is
accessible within LIGO as `Tezos.amount`.

To make sure we get paid, we will:

- calculate a `current_purchase_price` based on the
  [equation specified earlier](tezos-taco-shop-smart-contract.md#calculating-the-current-purchase-price)
- check if the sent amount matches the `current_purchase_price`:
  - if not, then our contract will fail (`failwith`)
  - otherwise, stock for the given `taco_kind` will be decreased and
    the payment accepted

<Syntax syntax="pascaligo">

```pascaligo group=b
function buy_taco (const taco_kind_index : nat ; var taco_shop_storage : taco_shop_storage) : return is
  block {
    // Retrieve the taco_kind from the contract's storage or fail
    var taco_kind : taco_supply :=
      case taco_shop_storage[taco_kind_index] of
        Some (kind) -> kind
      | None -> (failwith ("Unknown kind of taco.") : taco_supply)
      end;

    const current_purchase_price : tez =
      taco_kind.max_price / taco_kind.current_stock;

    if Tezos.amount =/= current_purchase_price then
      // We won't sell tacos if the amount is not correct
      failwith ("Sorry, the taco you are trying to purchase has a different price");
    else skip;

    // Decrease the stock by 1n, because we have just sold one
    taco_kind.current_stock := abs (taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind
  } with ((nil : list (operation)), taco_shop_storage)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let buy_taco (taco_kind_index, taco_shop_storage : nat * taco_shop_storage) : return =
  (* Retrieve the taco_kind from the contract's storage or fail *)
  let taco_kind : taco_supply =
    match Map.find_opt (taco_kind_index) taco_shop_storage with
    | Some k -> k
    | None -> (failwith "Unknown kind of taco" : taco_supply)
  in
  let current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock in
  (* We won't sell tacos if the amount is not correct *)
  let () = if Tezos.amount <> current_purchase_price then
    failwith "Sorry, the taco you are trying to purchase has a different price"
  in
  (* Update the storage decreasing the stock by 1n *)
  let taco_shop_storage = Map.update
    taco_kind_index
    (Some { taco_kind with current_stock = abs (taco_kind.current_stock - 1n) })
    taco_shop_storage
  in
  (([]: operation list), taco_shop_storage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let buy_taco = ((taco_kind_index, taco_shop_storage) : (nat, taco_shop_storage)) : return => {
  /* Retrieve the taco_kind from the contract's storage or fail */
  let taco_kind : taco_supply =
    switch (Map.find_opt (taco_kind_index, taco_shop_storage)) {
    | Some k => k
    | None => (failwith ("Unknown kind of taco"): taco_supply) } ;
  let current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock ;
  /* We won't sell tacos if the amount is not correct */
  let x : unit = if (Tezos.amount != current_purchase_price) {
    failwith ("Sorry, the taco you are trying to purchase has a different price")
  } ;
  /* Update the storage decreasing the stock by 1n */
  let taco_shop_storage = Map.update (
    taco_kind_index,
    (Some ({...taco_kind, current_stock : abs (taco_kind.current_stock - 1n) })),
    taco_shop_storage );
  (([]: list(operation)), taco_shop_storage)
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let buy_taco3 = ([taco_kind_index, taco_shop_storage] : [nat, taco_shop_storage]) : return_ => {
  /* Retrieve the taco_kind from the contracts storage or fail */
  let taco_kind : taco_supply =
    match (Map.find_opt (taco_kind_index, taco_shop_storage), {
      Some: (k:taco_supply) => k,
      None: (_:unit) => (failwith ("Unknown kind of taco") as taco_supply)
    }) ;
  let current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock ;
  /* We won't sell tacos if the amount is not correct */
  if (Tezos.amount != current_purchase_price) {
    failwith ("Sorry, the taco you are trying to purchase has a different price") as return_
  } else {
    /* Update the storage decreasing the stock by 1n */
    let taco_shop_storage = Map.update (
      taco_kind_index,
      (Some (({...taco_kind, current_stock : abs (taco_kind.current_stock - (1 as nat)) }))),
      taco_shop_storage );
    return [(list([]) as list <operation>), taco_shop_storage]
  }
};
```

</Syntax>

Now let's test our function against a few inputs using the LIGO test framework.
For that, we will have another file in which will describe our test:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=test
#include "gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.ligo"

function assert_string_failure (const res : test_exec_result ; const expected : string) : unit is
  block {
  const expected = Test.eval(expected) ;
  } with
    case res of
    | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal (actual, expected))
    | Fail (Other) -> failwith ("contract failed for an unknown reason")
    | Success -> failwith ("bad price check")
    end

const test = block {
  // Originate the contract with a initial storage
  const init_storage =
    map [
      1n -> record [ current_stock = 50n ; max_price = 50tez ] ;
      2n -> record [ current_stock = 20n ; max_price = 75tez ] ; ];
  const (pedro_taco_shop_ta, _code, _size) = Test.originate(buy_taco, init_storage, 0tez) ;
  // Convert typed_address to contract
  const pedro_taco_shop_ctr = Test.to_contract (pedro_taco_shop_ta);
  // Convert contract to address
  const pedro_taco_shop = Tezos.address (pedro_taco_shop_ctr);

  // Test inputs
  const classico_kind = 1n ;
  const unknown_kind = 3n ;

  // Auxiliary function for testing equality in maps
  function eq_in_map (const r : taco_supply; const m : taco_shop_storage; const k : nat) is block {
    var b := case Map.find_opt(k, m) of
    | None -> False
    | Some (v) -> (v.current_stock = r.current_stock) and (v.max_price = r.max_price)
    end
  } with b  ;

  // Purchasing a Taco with 1tez and checking that the stock has been updated
  const ok_case : test_exec_result = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 1tez) ;
  const _unit = case ok_case of
    | Success  -> block {
      const storage = Test.get_storage (pedro_taco_shop_ta) ;
    } with (assert (eq_in_map (record [ current_stock = 49n ; max_price = 50tez ], storage, 1n) and
                    eq_in_map (record [ current_stock = 20n ; max_price = 75tez ], storage, 2n)))
    | Fail (x) -> failwith ("ok test case failed")
  end ;

  // Purchasing an unregistred Taco
  const nok_unknown_kind = Test.transfer_to_contract (pedro_taco_shop_ctr, unknown_kind, 1tez) ;
  const _u = assert_string_failure (nok_unknown_kind, "Unknown kind of taco") ;

  // Attempting to Purchase a Taco with 2tez
  const nok_wrong_price = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 2tez) ;
  const _u = assert_string_failure (nok_wrong_price, "Sorry, the taco you are trying to purchase has a different price") ;
  } with unit
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=test
#include "gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.mligo"

let assert_string_failure (res : test_exec_result) (expected : string) : unit =
  let expected = Test.eval expected in
  match res with
  | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal actual expected)
  | Fail (Other) -> failwith "contract failed for an unknown reason"
  | Success -> failwith "bad price check"

let test =
  (* originate the contract with a initial storage *)
  let init_storage = Map.literal [
      (1n, { current_stock = 50n ; max_price = 50tez }) ;
      (2n, { current_stock = 20n ; max_price = 75tez }) ; ]
  in
  let (pedro_taco_shop_ta, _code, _size) = Test.originate buy_taco init_storage 0tez in
  (* Convert typed_address to contract *)
  let pedro_taco_shop_ctr = Test.to_contract pedro_taco_shop_ta in
  (* Convert contract to address *)
  let pedro_taco_shop = Tezos.address (pedro_taco_shop_ctr) in

  (* Test inputs *)
  let classico_kind = 1n in
  let unknown_kind = 3n in

  (* Auxiliary function for testing equality in maps *)
  let eq_in_map (r : taco_supply) (m : taco_shop_storage) (k : nat) =
    match Map.find_opt k m with
    | None -> false
    | Some v -> v.current_stock = r.current_stock && v.max_price = r.max_price in

  (* Purchasing a Taco with 1tez and checking that the stock has been updated *)
  let ok_case : test_exec_result = Test.transfer_to_contract pedro_taco_shop_ctr classico_kind 1tez in
  let () = match ok_case with
    | Success  ->
      let storage = Test.get_storage pedro_taco_shop_ta in
      assert ((eq_in_map { current_stock = 49n ; max_price = 50tez } storage 1n) &&
              (eq_in_map { current_stock = 20n ; max_price = 75tez } storage 2n))
    | Fail x -> failwith ("ok test case failed")
  in

  (* Purchasing an unregistred Taco *)
  let nok_unknown_kind = Test.transfer_to_contract pedro_taco_shop_ctr unknown_kind 1tez in
  let () = assert_string_failure nok_unknown_kind "Unknown kind of taco" in

  (* Attempting to Purchase a Taco with 2tez *)
  let nok_wrong_price = Test.transfer_to_contract pedro_taco_shop_ctr classico_kind 2tez in
  let () = assert_string_failure nok_wrong_price "Sorry, the taco you are trying to purchase has a different price" in
  ()
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=test
#include "gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.religo"

let assert_string_failure = ((res,expected) : (test_exec_result, string)) : unit => {
  let expected = Test.eval (expected) ;
  switch (res) {
  | Fail (Rejected (actual,_)) => assert (Test.michelson_equal (actual, expected))
  | Fail (Other) => failwith ("contract failed for an unknown reason")
  | Success => failwith ("bad price check")
  }
} ;

let test =
  /* originate the contract with a initial storage */
  let init_storage = Map.literal ([
      (1n, { current_stock : 50n , max_price : 50tez }) ,
      (2n, { current_stock : 20n , max_price : 75tez }) , ]) ;
  let (pedro_taco_shop_ta, _code, _size) = Test.originate (buy_taco, init_storage, 0tez) ;
  /* Convert typed_address to contract */
  let pedro_taco_shop_ctr = Test.to_contract (pedro_taco_shop_ta);
  /* Convert contract to address */
  let pedro_taco_shop = Tezos.address (pedro_taco_shop_ctr);

  /* Test inputs */
  let classico_kind = 1n ;
  let unknown_kind = 3n ;

  /* Auxiliary function for testing equality in maps */
  let eq_in_map = ((r, m, k) : (taco_supply, taco_shop_storage, nat)) : bool =>
    switch (Map.find_opt (k, m)) {
    | None => false
    | Some (v) => v.current_stock == r.current_stock && v.max_price == r.max_price
    };

  /* Purchasing a Taco with 1tez and checking that the stock has been updated */
  let ok_case : test_exec_result = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 1tez) ;
  let _u = switch (ok_case) {
    | Success  =>
      let storage = Test.get_storage (pedro_taco_shop_ta) ;
      assert (eq_in_map({ current_stock : 49n , max_price : 50tez }, storage, 1n) &&
              eq_in_map({ current_stock : 20n , max_price : 75tez }, storage, 2n))
    | Fail (x) => failwith ("ok test case failed")
  } ;

  /* Purchasing an unregistred Taco */
  let nok_unknown_kind = Test.transfer_to_contract (pedro_taco_shop_ctr, unknown_kind, 1tez) ;
  let _u = assert_string_failure (nok_unknown_kind, "Unknown kind of taco") ;

  /* Attempting to Purchase a Taco with 2tez */
  let nok_wrong_price = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 2tez) ;
  let _u = assert_string_failure (nok_wrong_price, "Sorry, the taco you are trying to purchase has a different price") ;
  ()
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=test
#include "gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.jsligo"

let assert_string_failure = ([res,expected] : [test_exec_result, string]) : unit => {
  let expected = Test.eval (expected) ;
  match (res, {
    Fail: (x: test_exec_error) => (
      match (x, {
        Rejected: (x:[michelson_code,address]) => assert (Test.michelson_equal (x[0], expected)),
        Other: (_:unit) => failwith ("contract failed for an unknown reason")
      })),
    Success: (_:unit) => failwith ("bad price check")
  } );
} ;

let _test = (_: unit): unit => {
  /* Originate the contract with a initial storage */
  let init_storage = Map.literal (list([
      [1 as nat, { current_stock : 50 as nat, max_price : 50 as tez }],
      [2 as nat, { current_stock : 20 as nat, max_price : 75 as tez }] ])) ;
  let [pedro_taco_shop_ta, _code, _size] = Test.originate (buy_taco, init_storage, 0 as tez) ;
  /* Convert typed_address to contract */
  let pedro_taco_shop_ctr = Test.to_contract (pedro_taco_shop_ta);
  /* Convert contract to address */
  let pedro_taco_shop = Tezos.address (pedro_taco_shop_ctr);

  /* Test inputs */
  let classico_kind = (1 as nat) ;
  let unknown_kind = (3 as nat) ;

  /* Auxiliary function for testing equality in maps */
  let eq_in_map = ([r, m, k] : [taco_supply, taco_shop_storage, nat]) : bool =>
    match(Map.find_opt(k, m), {
     None: () => false,
     Some: (v : taco_supply) => v.current_stock == r.current_stock && v.max_price == r.max_price }) ;

  /* Purchasing a Taco with 1tez and checking that the stock has been updated */
  let ok_case : test_exec_result = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 1 as tez) ;
  let _u = match (ok_case, {
    Success: (_:unit) => {
      let storage = Test.get_storage (pedro_taco_shop_ta) ;
      assert (eq_in_map({ current_stock : 49 as nat, max_price : 50 as tez }, storage, 1 as nat) &&
              eq_in_map({ current_stock : 20 as nat, max_price : 75 as tez }, storage, 2 as nat)); },
    Fail: (_: test_exec_error) => failwith ("ok test case failed")
  }) ;

  /* Purchasing an unregistred Taco */
  let nok_unknown_kind = Test.transfer_to_contract (pedro_taco_shop_ctr, unknown_kind, 1 as tez) ;
  let _u2 = assert_string_failure (nok_unknown_kind, "Unknown kind of taco") ;

  /* Attempting to Purchase a Taco with 2tez */
  let nok_wrong_price = Test.transfer_to_contract (pedro_taco_shop_ctr, classico_kind, 2 as tez) ;
  let _u3 = assert_string_failure (nok_wrong_price, "Sorry, the taco you are trying to purchase has a different price") ;
  return unit
}

let test = _test (unit)
```

</Syntax>

Let's break it down a little bit:
- we include the file corresponding to the smart contract we want to
  test;
- we define `assert_string_failure`, a function reading a transfer
  result and testing against a failure. It also compares the failing
  data - here, a string - to what we expect it to be;
- `test` is actually performing the tests: Originates the taco-shop
  contract; purchasing a Taco with 1tez and checking that the stock
  has been updated ; attempting to purchase a Taco with 2tez and
  trying to purchase an unregistered Taco. An auxiliary function to
  check equality of values on maps is defined.

> checkout the [reference page](../../reference/test.md) for a more detailed description of the Test API

Now it is time to use the LIGO command `test`. It will evaluate our
smart contract and print the result value of those entries that start
with `"test"`:

<Syntax syntax="pascaligo">

```zsh
ligo run test gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-test.ligo
# Output:
#
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>
<Syntax syntax="cameligo">

```zsh
ligo run test gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-test.mligo
# Output:
#
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>
<Syntax syntax="reasonligo">

```zsh
ligo run test gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-test.religo
# Output:
#
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>
<Syntax syntax="jsligo">

```zsh
ligo run test gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-test.jsligo
# Output:
#
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>


**The test passed ! That's it - Pedro can now sell tacos on-chain, thanks to Tezos & LIGO.**

---

## 💰 Bonus: *Accepting Tips above the Taco Purchase Price*

If you would like to accept tips in your contract, simply change the
following line, depending on your preference.

**Without tips**

<Syntax syntax="pascaligo">

```pascaligo skip
if Tezos.amount =/= current_purchase_price then
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
if Tezos.amount <> current_purchase_price then
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
if (Tezos.amount != current_purchase_price)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
if (Tezos.amount != current_purchase_price)
```

</Syntax>

**With tips**

<Syntax syntax="pascaligo">

```pascaligo skip
if Tezos.amount < current_purchase_price then
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
if Tezos.amount >= current_purchase_price then
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
if (Tezos.amount >= current_purchase_price)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
if (Tezos.amount >= current_purchase_price)
```

</Syntax>
