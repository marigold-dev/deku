---
sidebar_position: 4
---

# Handling tickets in Deku

Unlike Tezos, Deku does not have an native currency, but tickets. These tickets are directly owned
by the accounts, and can be transferred between accounts or directly sent to smart contracts using
the client. In this chapter, we show how to mint and send tickets on Deku.

## Minting tickets in Deku

Here is an example, written in JSLigo, of a smart contract that expects `bytes` and mints a `byte
ticket` before sending it back to the sender:

```jsligo
type storage = unit;

const transfer_ticket = (ticket:ticket<bytes>, target: address) => {
  return match(Tezos.get_contract_opt (target), {
    None: () => (failwith("Incorrect address")),
    Some: (c:contract<unit>) => {
      return Tezos.transaction (ticket, (0 as mutez), c);
    }
  });
};

const main = (payload: bytes, storage: storage) => {
  let ticket = Tezos.create_ticket (payload, (100 as nat));
  let op = transfer_ticket(ticket, Tezos.get_sender ());
  return [list([op]), storage];
}
```

This contract is fairly straightforward: the `storage` does not change, and the `main` function simply takes the bytes `payload` from the sender, uses it to mint 100 tickets and send those to the sender. As on Tezos, implicit accounts are represented as contracts of the `unit` type.

Here is how to compile and originate the contract on Deku:

```bash
$ ligo compile contract ticket.jsligo > ticket.tz
$ deku-cli originate wallet.json ticket.tz 'Unit'
operation hash: Do3UPjj84fPZnsR43shUUUhaj3pRJjbXrketwNThEwjJctAUgHAE
Contract originated at address DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz
```

This contract can now be called to mint some tickets:

```bash
$ deku-cli invoke --endpoint http://0.0.0.0:8080 wallet.json DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz '0x'
operation hash: Do2MZ9egKWCtEXz7MLVcDFGvB19CHaAiaJg5nXU8we5ey4HXL8UJ
$ deku-cli show-balance tz1Vhy8BWXdQNm6wNFi36hWj4iSoGqBrMB4v
[
  {
    ticket: { ticketer: 'DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz', data: '' },
    amount: 100
  }
]
```

Fresh tickets, nice!

## Consuming tickets in a smart contract

Let's take a look at tickets consumption by smart contracts. Here's an example of contract that
waits
for a ticket and an address, stores the ticket, and sends the ticket to the user when they request
it.

```jsligo
type deposit = option<[address, ticket<bytes>]>;

type storage = {
  deposit: deposit
};

type entrypoint =
  | ["Deposit", ticket<bytes>, address]
  | ["Withdraw"]

const transfer_ticket = (ticket:ticket<bytes>, target: address) => {
  return match(Tezos.get_contract_opt (target), {
    None: () => (failwith("Incorrect address")),
    Some: (c:contract<unit>) => {
      return Tezos.transaction (ticket, (0 as mutez), c);
    }
  });
};

const main = (action: entrypoint, storage: storage) => {
  return match(action, {
    Deposit: (deposit:[ticket<bytes>, address]) => {
      let [ticket, to_] = deposit;
      assert_none(storage.deposit);
      let [[_ticketer, [_data, _amount]], ticket] = Tezos.read_ticket(ticket);
      let deposit = Some([to_, ticket]);
      return [list([]), {deposit}];
    },
    Withdraw: () => {
      let sender = Tezos.get_sender();
      let [address, ticket] = Option.unopt_with_error(
        storage.deposit, "Nothing to withdraw."
      );
      assert_with_error(address == sender, "You're not allowed to withdraw");
      let transaction = transfer_ticket(ticket, address);
      return [list([transaction]), {deposit:None()}];
    }
  });
}
```
First, notice that this contract's storage is more involved than the previous one, as we need to
store an address and a ticket so that another user can retrieve it. The `main` function waits for a
deposit when the storage is `None`, and a withdraw otherwise.

Let's name this contract `vault.jsligo`. Compilation and origination are the same as before:

```bash
$ ligo compile contract vault.jsligo > vault.tz
$ deku-cli originate wallet.json vault.tz 'None'
operation hash: Do2p2Le3he2kwnng7SKcizNq3715FcsaysCohFrPgVd9zFNfwc56
Contract originated at address DK141PdoGDp9gZea9okEiPJfL3Y9z5GhhZ5T
```

First, let's inspect this contract's storage:

```bash
$ deku-cli show-storage DK141PdoGDp9gZea9okEiPJfL3Y9z5GhhZ5T
{ none: true }
```

This prints the VM's state for this contract, which does not follow Tezos and Michelson's
conventions. However it seems to match the value we provided when we originated — so far, so good.

Let's make a deposit. Unfortunately, we can't ask the Ligo compiler to produce the command line
argument for us, nor we can use the `invoke-ligo` endpoint: indeed, for the moment there is no way
to represent a ticket expression in Ligo to give to these commands! We thus have to write it by
hand.

We know we want to call the first entrypoint, so the expression has to start with `Left`. We also
want to give two arguments: the ticket itself (with an amount) and the deposit address, so we'll
have to use `Pair`s. The ticket is designated by the minting contract (called the "ticketer"), the
data bytes and an amount. Using the ticket from the previous contract, we get the following
Michelson expression:

```
'Left (Pair (Pair "DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz" 0x 10) "tz1Vhy8BWXdQNm6wNFi36hWj4iSoGqBrMB4v")'
```

As noted in the previous chapter though, in order for Deku to accept to change our balance, we have
to explicitely give permission to handle this ticket, by writing its information again after the
`invoke` argument. Finally this gives us the following command:

```bash
$ deku-cli invoke wallet.json DK141PdoGDp9gZea9okEiPJfL3Y9z5GhhZ5T \
  'Left (Pair (Pair "DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz" 0x 10) \
  "tz1Vhy8BWXdQNm6wNFi36hWj4iSoGqBrMB4v")' \
  '(Pair "DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz" 0x 10)'
```

This command can be made shorter using a variable:

```bash
$ TICKET='(Pair "DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz" 0x 10)'
$ deku-cli invoke  wallet.json DK141PdoGDp9gZea9okEiPJfL3Y9z5GhhZ5T \
  "Left (Pair $TICKET \"tz1Vhy8BWXdQNm6wNFi36hWj4iSoGqBrMB4v\")" "$TICKET"
```

Let's check out balance to see if the deposit worked:

```bash
$ deku-cli show-balance tz1Vhy8BWXdQNm6wNFi36hWj4iSoGqBrMB4v
[
  {
    ticket: { ticketer: 'DK1JxMLmohbyKXbGhQWLJZyktSsZKGxaM6jz', data: '' },
    amount: 90
  }z
]
```
