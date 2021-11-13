---
id: tezos-taco-shop-payout
title: Paying out profits from the Taco Shop
---

In the
[previous tutorial](tezos-taco-shop-smart-contract.md)
we have learnt how to setup & interact with the LIGO CLI. Followed an
implementation of a simple Taco Shop smart contract for our
entrepreneur Pedro.

In this tutorial we will make sure Pedro has access to tokens that
people have spent at his shop when buying tacos.

<br/>
<img src="/img/tutorials/get-started/tezos-taco-shop-payout/get-money.svg" width="50%" />

<div style={{ opacity: 0.7, textAlign: 'center', fontSize: '10px' }}>
<div>Icons made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/"                 title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/"                 title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>


## Analysing the Current Contract

### **`taco-shop.ligo`**

```pascaligo group=a
type taco_supply is record [
  current_stock : nat;
  max_price     : tez
]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

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

    if amount =/= current_purchase_price then
      // We won't sell tacos if the amount is not correct
      failwith ("Sorry, the taco you are trying to purchase has a different price");
    else skip;

    // Decrease the stock by 1n, because we have just sold one
    taco_kind.current_stock := abs (taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind
  } with ((nil : list (operation)), taco_shop_storage)
```

### Purchase Price Formula

Pedro's Taco Shop contract currently enables customers to buy tacos,
at a price based on a simple formula.

```pascaligo skip
const current_purchase_price : tez =
  taco_kind.max_price / taco_kind.current_stock
```

### Replacing *spendable* Smart Contracts

However, due to the
[recent protocol upgrade](http://tezos.gitlab.io/protocols/004_Pt24m4xi.html)
of the Tezos `mainnet`, Pedro cannot access the tokens stored in his
shop's contract directly. This was previously possible via *spendable
smart contracts*, which are no longer available in the new
protocol. We will have to implement a solution to access tokens from
the contract programmatically.

---

## Designing a Payout Scheme

Pedro is a standalone business owner, and in our case, he does not
have to split profits and earnings of the taco shop with anyone. So
for the sake of simplicity, we will payout all the earned XTZ directly
to Pedro right after a successful purchase.

This means that after all the *purchase conditions* of our contract
are met, e.g., the correct amount is sent to the contract, we will not
only decrease the supply of the individual purchased *taco kind*, but
we will also transfer this amount in a *subsequent transaction* to
Pedro's personal address.

## Forging a Payout Transaction

### Defining the Recipient

In order to send tokens, we will need a receiver address, which, in
our case, will be Pedro's personal account. Additionally we will wrap
the given address as a *`contract (unit)`*, which represents either a
contract with no parameters, or an implicit account.

```pascaligo group=ex1
const ownerAddress : address = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address);
const receiver : contract (unit) =
  case (Tezos.get_contract_opt (ownerAddress): option(contract(unit))) of
    Some (contract) -> contract
  | None -> (failwith ("Not a contract") : (contract(unit)))
  end;
```

> Would you like to learn more about addresses, contracts and
> operations in LIGO? Check out the
> [LIGO cheat sheet](api/cheat-sheet.md)

### Adding the Transaction to the List of Output Operations

Now we can transfer the amount received by `buy_taco` to Pedro's
`ownerAddress`. We will do so by forging a `transaction (unit, amount,
receiver)` within a list of operations returned at the end of our
contract.

```pascaligo group=ex1
const payoutOperation : operation = Tezos.transaction (unit, amount, receiver) ;
const operations : list (operation) = list [payoutOperation];
```

---

## Finalising the Contract

### **`taco-shop.ligo`**

```pascaligo group=b
type taco_supply is record [
  current_stock : nat;
  max_price     : tez
]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

const ownerAddress : address =
  ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address)

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
    taco_shop_storage[taco_kind_index] := taco_kind;

    const receiver : contract (unit) =
      case (Tezos.get_contract_opt (ownerAddress): option(contract(unit))) of
        Some (contract) -> contract
      | None -> (failwith ("Not a contract") : (contract(unit)))
      end;

    const payoutOperation : operation = Tezos.transaction (unit, amount, receiver);
    const operations : list(operation) = list [payoutOperation]
  } with ((operations : list (operation)), taco_shop_storage)
```

### Dry-run the Contract

To confirm that our contract is valid, we can dry-run it. As a result,
we see a *new operation* in the list of returned operations to be
executed subsequently.

```pascaligo skip
ligo run dry-run taco-shop.ligo --syntax pascaligo --amount 1 --entry-point buy_taco 1n "map [
   1n -> record [
           current_stock = 50n;
           max_price = 50tez
         ];
   2n -> record [
           current_stock = 20n;
           max_price = 75tez
         ];
]"
```

<img src="/img/tutorials/get-started/tezos-taco-shop-payout/dry-run-1.png" />
<div style={{ opacity: 0.7, textAlign: 'center', fontSize: '12px', marginTop: '-24px' }}>
<b>Operation(...bytes)</b> included in the output
</div>

<br/>

**Done! Our tokens are no longer locked in the contract, and instead
  they are sent to Pedro's personal account/wallet.**

---

## 👼 Bonus: Donating Part of the Profits

Because Pedro is a member of the Speciality Taco Association (STA), he
has decided to donate **10%** of the earnings to the STA. We will just
add a `donationAddress` to the contract, and compute a 10% donation
sum from each taco purchase.

```pascaligo group=bonus
const ownerAddress : address = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address);
const donationAddress : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address);

const receiver : contract (unit) =
  case (Tezos.get_contract_opt (ownerAddress) : option(contract(unit))) of
    Some (contract) -> contract
  | None -> (failwith ("Not a contract") : contract (unit))
  end;
const donationReceiver : contract (unit) =
  case (Tezos.get_contract_opt (donationAddress) : option(contract(unit))) of
    Some (contract) -> contract
  | None  -> (failwith ("Not a contract") : contract (unit))
  end;

const donationAmount : tez = Tezos.amount / 10n;

const operations : list (operation) = list [
  // Pedro will get 90% of the amount
  transaction (unit, Tezos.amount - donationAmount, receiver);
  transaction (unit, donationAmount, donationReceiver)
];
```

This will result into two operations being subsequently executed on the blockchain:
- Donation transfer (10%)
- Pedro's profits (90%)
