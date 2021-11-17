---
id: security
title: Smart contract security
---

import Syntax from '@theme/Syntax';

In this article, we will cover the basics of Tezos smart contract security. We will describe several potential vulnerabilities that stem from developers' misconceptions about the distributed nature of blockchains. We will also suggest ways to protect your contracts against these kinds of attacks.

**Disclaimer:**
1. This guide is aimed at giving the reader an overview of popular attacks on smart contracts and distributed applications. It is not an exhaustive list of all the possible attack vectors. Please, use your own judgement.
2. The descriptions in this document are valid for the protocol 008_PtEdo2Zk (Edo). Since Tezos is an upgradeable blockchain, some of the blockchain mechanics may change in case a new proposal is adopted.

## Resource constraints

Tezos limits the resources available to the contracts. It bounds operations size so that nodes can broadcast the operations over the network in a reasonable time. It also places a limit on the computations the bakers need to perform to validate an operation – the **gas limit.** When you develop your contract, you need to bear these limits in mind.

Let us look at a seemingly innocent wallet contract that stores an event log:

<Syntax syntax="pascaligo">

```pascaligo
type parameter is Fund | Send of address * tez

type tx is Incoming of address * tez | Outgoing of address * tez

type storage is record [owner : address; transactionLog : list (tx)]

function send (const dst : address; const amount_ : tez) is
block {
  const callee : option (contract (unit)) = Tezos.get_contract_opt (dst)
} with
    case callee of [
      Some (contract) ->
        block {
          const op = Tezos.transaction (Unit, amount_, contract)
        } with (Outgoing (dst, amount_), list [op])
    | None -> (failwith ("Could not send tokens") : tx * list (operation))
    ]

function receive (const src : address; const amount_ : tez) is
  (Incoming (src, amount_), (list [] : list (operation)))

function main (const p : parameter; const s : storage) is
block {
  const result
  = case p of [
      Fund -> receive (Tezos.sender, Tezos.amount)
    | Send (args) ->
        block {
          assert (Tezos.sender = s.owner and Tezos.amount = 0mutez)
        } with send (args)
    ];
  const tx = result.0;
  const ops = result.1
} with (ops, s with record [transactionLog = tx # s.transactionLog])
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type parameter = Fund | Send of address * tez

type transaction = Incoming of address * tez | Outgoing of address * tez

type storage = {owner : address; transactionLog : transaction list}

let send (dst, amount_ : address * tez) =
  let callee : unit contract option = Tezos.get_contract_opt dst in
  match callee with
    Some contract ->
      let op = Tezos.transaction () amount_ contract in
      Outgoing (dst, amount_), [op]
  | None -> (failwith "Could not send tokens" : transaction * operation list)

let receive (from, amount_ : address * tez) =
  Incoming (from, amount_), ([] : operation list)

let main (p, s : parameter * storage) =
  let tx, ops =
    match p with
      Fund -> receive (Tezos.sender, Tezos.amount)
    | Send args ->
        let u = assert (Tezos.sender = s.owner && Tezos.amount = 0mutez) in
        send args in
  ops, {s with transactionLog = tx :: s.transactionLog}
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type parameter = Fund | Send((address, tez));

type transaction = Incoming((address, tez)) | Outgoing((address, tez));

type storage = {owner: address, transactionLog: list(transaction) };

let send = ((dst, amount_): (address, tez)) => {
  let callee: option(contract(unit)) = Tezos.get_contract_opt(dst);
  switch(callee){
  | Some (contract) =>
      {
        let op = Tezos.transaction((), amount_, contract);
        (Outgoing (dst, amount_), [op])
      }
  | None => (failwith("Could not send tokens") : (transaction, list(operation)))
  }
};

let receive = ((from, amount_): (address, tez)) =>
  (Incoming (from, amount_), ([] : list(operation)));

let main = ((p, s): (parameter, storage)) => {
  let tx, ops =
    switch(p){
    | Fund => receive(Tezos.sender, Tezos.amount)
    | Send(args) =>
        {
          assert(Tezos.sender == s.owner && Tezos.amount == 0mutez);
          send(args)
        }
    };
  (ops, {...s, transactionLog: [tx, ...s.transactionLog]})
};

```

</Syntax>

This contract:
1. Can receive funds sent to it via the `Fund` entrypoint.
2. Can send some tez via the `Send` entrypoint callable by the owner.
3. Stores a log of all the operations.

What can go wrong? To answer this question, we will need to dive a bit into how Tezos processes transactions and what limits it places on them.

To guarantee that the nodes spend reasonable time processing transactions, Tezos requires that the execution consumes no more than a certain amount of _gas_ (in the current protocol, it is 1 040 000 gas units).

But in Tezos, the amount of gas consumed depends on the size of the storage! All non-lazy (i.e. non-BigMap) storage entries get fetched, deserialised, and type-checked upon each contract invocation. It means that:
1. Our contract will be more and more expensive to call with every transaction made.
2. Eventually, when the gas consumption is too high, every transaction will hit the upper bound, which will render the contract unusable.

In this particular case the best solution would be to use an off-chain indexer that would monitor and record the transactions to the contract. If you are sure you need an event log in the contract storage, you should at least store the logs in a big map, e.g., indexed incrementally.

Generally, you need to think about whether the side effect of gas consumption can halt the execution prematurely. Here are the tips that can help you reduce the risk of potential gas exhaustion.
1. Limit the size of non-lazy storage:
   - Do not store data extendable by the users (e.g., event logs, a set of token holders) in non-lazy containers.
   - If using non-lazy containers is absolutely required, place an upper bound on the size of non-lazy containers.
   - Limit the maximum size of strings and byte strings.
   - Do not put untrusted lambdas in storage.
   - Be careful with all unbounded types, including `nat`, `int`, etc. Although exploiting gas exhaustion attacks with non-container types may be harder, it is still possible.
2. Ensure that your contract logic does not allow attackers to increase the interpretation cost, e.g., by forcing future transactions to run a huge loop.

## Transaction ordering
It is crucial to understand that all blockchains, including Tezos, are distributed systems where block producers – bakers in Tezos – are free to include, censor, and reorder transactions within a block. For most of the practical applications, this does not pose a threat. However, in some cases, especially in Decentralised Finance (DeFi) applications, bakers can use their power to gain economic benefit from reordering or censoring out user transactions.

Aside from bakers, other actors can indirectly influence the transaction ordering as well. Attackers can set higher fees or use accounts with lower counter values to make bakers put the attackers' transactions in front of others.

A classic example of a system vulnerable to this kind of attacks is a decentralised exchange with an on-chain orderbook, like this one (let us assume just one asset pair for clarity):

<Syntax syntax="pascaligo">

```pascaligo skip
type order is record [price : nat; volume : nat]

type storage is record [bids : list (order); asks : list (order)]

type parameter is Buy of order | Sell of order

function buy (const order : order; var s : storage) is ...
function sell (const order : order; var s : storage) is ...
function main (const p : parameter; var s : storage) is ...
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
type order = {price : nat; volume : nat}

type storage = {bids : order list; asks : order list}

type parameter = Buy of order | Sell of order

let buy (order, s : order * storage) = ...
let sell (order, s : order * storage) = ...
let main (p, s : parameter * storage) = ...
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
type order = {price: nat, volume: nat };

type storage = {bids: list(order), asks: list(order) };

type parameter = Buy(order) | Sell(order);

let buy = ((order, s) : (order, storage)) => ...
let sell = ((order, s) : (order, storage)) => ...
let main = ((p, s) : (parameter, storage)) => ...
```

</Syntax>

An attacker may notice some transaction, for example, a request to buy some big volume of asset. They may then _front-run_ this transaction and, anticipating the price going up, insert a _buy_ order at the current price before the trader's transaction. Thus, they can benefit from the price change by selling the asset at a higher price.

In fact, if the front-runner is a baker, the so-called _miner extracted value_ [poses a big risk](https://arxiv.org/pdf/1904.05234.pdf) to security of blockchains in general. You should avoid letting miners get rewards from transaction ordering. In this particular case, moving the order book off-chain would be a good option.

## Timestamps

Aside from transaction ordering, bakers can manipulate other variables you might want to rely on. A classic example of such a value is `Tezos.now`. Previously, it used to be equal to the current block timestamp. This behaviour has been changed to eliminate straightforward manipulations. Since Tezos is a distributed system, there is no way to make sure the block was produced _exactly_ at the specified time. Thus, bakers could slightly adjust the timestamp to make a transaction produce a different result.

In the current protocol, `Tezos.now` is equal to the _previous_ block timestamp plus a fixed value. Although `Tezos.now` becomes less manipulable with this new behaviour, the only assumption you can make is that the operation goes through _roughly about_ the specified timestamp. And, of course, you should never use `Tezos.now` as a source of randomness.

## Reentrancy and call injection

Tezos features a rather unconventional model of execution:
1. The contract state is updated _after_ the computations are completed.
2. The contracts cannot emit operations in the middle of execution.
3. Internal operations are _queued._

The first two points resemble the Checks-Effects-Interactions pattern popular in Solidity. In Ethereum, it is considered a best practice, and Tezos enforces this on the protocol level. Such restrictions help  prevent reentrancy attacks: if the state of your contract is updated _before_ someone makes a reentrant call, this call would be treated as a regular one and should do no harm.

Consider the following snippet in Solidity:
```
function withdraw(uint256 amount) {
  uint256 balance = balances[beneficiary];
  require(balance >= amount);
  uint256 new_balance = balance - amount;
  beneficiary.call.value(amount)();
  balances[beneficiary] = new_balance;
}
```

You may notice that the _effect_ of updating the storage happens after _interaction_ – transferring the `amount` to the beneficiary. This contract has a reentrancy vulnerability: the contract execution would get paused during the transfer, and the beneficiary can call `withdraw` again _before_ their balance is updated.

It is quite hard to repeat this attack on Tezos, where the contract storage is always updated _before_ any interactions:

<Syntax syntax="pascaligo">

```pascaligo
type storage is record [beneficiary : address; balances : map (address, tez)]

type parameter is tez * contract (unit)

function withdraw (const param : parameter; var s : storage) is
block {
  const amount_ = param.0;
  const beneficiary = param.1;
  const beneficiary_addr = Tezos.address (beneficiary);
  const balance_
  = case Map.find_opt (beneficiary_addr, s.balances) of [
      Some (v) -> v
    | None -> 0mutez
    ];
  if (balance < amount_) then failwith ("Insufficient balance") else skip;
  const new_balance = balance_ - amount_;
  const op = Tezos.transaction (Unit, amount_, beneficiary);
  s.balances [beneficiary_addr] := new_balance
} with (list [op], s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type storage = {beneficiary : address; balances : (address, tez) map}

type parameter = tez * (unit contract)

let withdraw (param, s : parameter * storage) =
  let amount_, beneficiary = param in
  let beneficiary_addr = Tezos.address beneficiary in
  let balance_ =
    match (Map.find_opt beneficiary_addr s.balances) with
      Some v -> v
    | None -> 0mutez in
  let new_balance =
    if (balance_ >= amount_)
    then balance_ - amount_
    else (failwith "Insufficient balance" : tez) in
  let op = Tezos.transaction () amount_ beneficiary in
  let new_balances =
    Map.update beneficiary_addr (Some new_balance) s.balances in
  [op], {s with balances = new_balances}
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type storage = {beneficiary: address, balances: map(address, tez) };

type parameter = (tez, contract(unit));

let withdraw = ((param, s): (parameter, storage)) => {
  let amount_, beneficiary = param;
  let beneficiary_addr = Tezos.address(beneficiary);
  let balance_ =
    switch(Map.find_opt(beneficiary_addr, s.balances)){
    | Some (v) => v
    | None => 0mutez
    };
  let new_balance =
    if (balance_ >= amount_) {
      balance_ - amount_
    } else {
      (failwith("Insufficient balance") : tez)
    };
  let op = Tezos.transaction((), amount_, beneficiary);
  let new_balances =
    Map.update(beneficiary_addr, (Some new_balance), s.balances);
  ([op], {...s, balances: new_balances})
};
```

</Syntax>

Notice that the code flow is similar: we first check whether the beneficiary has enough balance, then forge an operation that sends the money, and finally we update the balances mapping. The difference is that in Tezos the operations are not executed immediately: we store the operation and later return it as a result of the entrypoint. Hence, the balances are updated by the time the operation is executed, so the reentrancy attack is mitigated.

However, in some cases reentrancy attacks are still possible, especially if contracts are supposed to "wait" for a callback in an indeterminate state. If you, for example, choose to store balances in a separate contract, your execution flow will need a lot more interactions than sending one internal operation:

| Current call | Treasury state after | Queued operations |
|--------------|----------------------|-------------------|
| `Treasury %withdraw`   | Waiting for balances | [`Balances %getBalance`] |
| `Balances %getBalance` | Waiting for balances | [`Treasury %withdrawContinuation`] |
| `Treasury %withdrawContinuation` | Sent | [Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| Send tez to `Beneficiary` | Sent | [`Balances %setNewBalance`] |
| `Balances %setNewBalance` | Sent | |

In this example, the Treasury contract uses a callback mechanism to get the sender balance. In an intermediate state between `%withdraw` and `%withdrawContinuation`, the balances request has already been sent but the funds have not been withdrawn yet, and the balances have not been updated. This opens up a possibility for a call injection attack.

For example, here is what happens if an attacker tries to call `%withdraw` twice within a single transaction:

| Step | Current call | Queued operations |
|------|--------------|-------------------|
| 1 | `Evil %attack` | [`Treasury %withdraw`, `Treasury %withdraw`] |
| 2 | `Treasury %withdraw`  | [`Balances %getBalance`] |
| 3 | `Treasury %withdraw`  | [`Balances %getBalance`, `Balances %getBalance`] |
| 4 | `Balances %getBalance`| [`Balances %getBalance`, `Treasury %withdrawContinuation`] |
| 5 | `Balances %getBalance`| [`Treasury %withdrawContinuation`, `Treasury %withdrawContinuation`] |
| 6 | `Treasury %withdrawContinuation` | [`Treasury %withdrawContinuation`, Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 7 | `Treasury %withdrawContinuation` | [Send tez to `Beneficiary`, `Balances %setNewBalance`, Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 8 | Send tez to `Beneficiary` | [`Balances %setNewBalance`, Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 9 | `Balances %setNewBalance` | [Send tez to `Beneficiary`, `Balances %setNewBalance`] |
| 10 | Send tez to `Beneficiary` | [`Balances %setNewBalance`] |
| 11 | `Balances %setNewBalance` | |

The attacker successfully withdraws money twice using the fact that by the time the second `%withdraw` is called, the balance has not been updated yet.

## Transactions to untrusted contracts

When emitting a transaction to an untrusted contract, you can not assume that it will "play by the rules". Rather, you should always bear in mind that the callee may fail, causing the entire operation to fail, or emit other operations you do not expect.

Let us consider the following example:

<Syntax syntax="pascaligo">

```pascaligo
type storage is record [owner : address; beneficiaries : list (address)]

function send_rewards (const beneficiary_addr : address) is
block {
  const maybe_contract : option (contract (unit))
  = Tezos.get_contract_opt (beneficiary_addr);
  const beneficiary
  = case maybe_contract of [
      Some (contract) -> contract
    | None -> (failwith ("CONTRACT_NOT_FOUND") : contract (unit))
    ]
} with Tezos.transaction (Unit, 5000000mutez, beneficiary)

function main (const p : unit; const s : storage) is
  if Tezos.sender =/= s.owner
  then (failwith ("ACCESS_DENIED") : list (operation) * storage)
  else
    block {
      const ops = List.map (send_rewards, s.beneficiaries)
    } with (ops, s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type storage = {owner : address; beneficiaries : address list}

let send_rewards (beneficiary_addr : address) =
  let maybe_contract : unit contract option =
    Tezos.get_contract_opt beneficiary_addr in
  let beneficiary =
    match maybe_contract with
      Some contract -> contract
    | None -> (failwith "CONTRACT_NOT_FOUND" : unit contract) in
  Tezos.transaction () 5000000mutez beneficiary

let main (p, s : unit * storage) =
  if Tezos.sender <> s.owner
  then (failwith "ACCESS_DENIED" : operation list * storage)
  else
    let ops = List.map send_rewards s.beneficiaries in
    ops, s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type storage = {owner: address, beneficiaries: list(address) };

let send_rewards = (beneficiary_addr: address) => {
  let maybe_contract: option(contract(unit)) = 
    Tezos.get_contract_opt(beneficiary_addr);
  let beneficiary = 
    switch(maybe_contract){
    | Some contract => contract
    | None => (failwith("CONTRACT_NOT_FOUND") : contract(unit))
    };
  Tezos.transaction((), 5000000mutez, beneficiary)
};

let main = ((p, s): (unit, storage)) => 
  if (Tezos.sender != s.owner) {
    (failwith("ACCESS_DENIED") : (list(operation), storage))
  } else {
  
    let ops = List.map(send_rewards, s.beneficiaries);
    (ops, s)
  };
```

</Syntax>

The contract emits a bunch of operations that transfer 5 tez to each of the beneficiaries listed in storage. The flaw here is that one of the receiver contracts may fail, preventing others from receiving the reward. This may be intentional censorship or a bug in the receiver contract – in either case, the contract gets stuck.

Instead of making a batch transfer, it is better to let beneficiaries withdraw their funds individually. This way, if the receiver contract fails, it would not affect other withdrawals.

## Incorrect authorisation checks

When developing a contract, you may often want to restrict access to certain entrypoint. You need to somehow ensure that:
1. The request comes from an authorised entity
2. This entity cannot be tricked into sending this request.

You may be tempted to use `Tezos.source` instruction – it returns the address of an implicit account who injected the operation – but this violates our second requirement. It is easy to ask the owner of this implicit account to make a seemingly innocent transfer to a malicious contract that, in turn, emits an operation to a restricted entrypoint. The attacker contract may disguise itself as some blockchain game or a DAO, but neither the caller would be aware of its side-effects nor the callee would notice the presence of the intermediary. You should **never** use `Tezos.source` for authorisation purposes.

Checking whether `Tezos.sender` – the address of the immediate caller – is authorised to perform an operation is better: since the request comes directly from the authorised entity, we can be more certain this call is intended. Such an approach is a decent default choice if both conditions hold true:
1. The sender contract is well secured against emitting arbitrary operations. For instance, it must not contain ["view" entrypoints](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints) as defined in [TZIP-4](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md).
2. You only need to authorise an immediate caller and not the contracts somewhere up in the call chain.

If any of these conditions is not met, you need to use a more advanced technique called "tickets". Tickets are much like "contract signatures": a contract may issue a ticket that authorises a certain action. A ticket holds the data of any type, and a number – ticket _amount_. A ticket can not be copied but it can be split. If you split a ticket of amount `N`, you would get two tickets with amounts `M` and `K` such that `N = M + K`. You can also join two tickets if they have the same data and are issued by the same contract. In this case, you would get a new ticket with the sum of the amounts.

To check whether an action is authorised, you need to see if the ticket meets the following conditions:
1. The ticket issuer has enough permissions to perform this action.
2. The ticket amount and data are correct (the definition of "correct" is application-specific, e.g., the amount may mean the number of tokens to spend or the number of _times_ the action can be executed).

We recommend using the sender-based authorisation only in simple scenarios, e.g., when the contract has a single "owner" contract controlled by an implicit account. Otherwise, it is better to use ticket-based authorisation.
