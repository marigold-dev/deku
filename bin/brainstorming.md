- block
- wallet
- transactions
- swaps (cross-chain atomic swaps)

## State

- balance
- frozen blances
- validator list
- current block producer + timeout + round-robin

## Consensus

reasonable 50ms-100ms

- proof-of-authority

A : Alice
B : Bob

X : Continent Europe
Y : Continent America

message by X, signed by 2/3 of all nodes in X
so if 2/3 of X agrees on the block, X agreed on the block

X -> Y -> Z -> X
X¹ -> X² -> X³ -> X¹

X block for X -> X transanctions

A(X) -> B(X), consensus of X¹ and X²
A(X) -> B(Y), consensus of X and Y
A(X) -> C(Z), consensus of X and Y

G is global current-block-producer
G commits A(X) -> B(Y)

2 choices:

- G commits on local blocks + global transactions
- **G unfolds local blocks into transactions, + G commits on all transactions**

every 10 blocks, local commit to global.

2 models:
i. some state is purely local = G can not change it. so when you are on a local metanode, you have two balances: local balance and global balance. moving one to the other takes time.
ii. G can change everything. so regularly, data is locked, at which points no tx can happen.

i
never locked, so always quick. bad UX: user must know about local vs global.
worst-case local = 1 local round trip
worst-case global = 1 global round trip (1 local -> global + global -> global)

ii
sometimes locked, and intrinsic tradeoffs: either stuff is locked regularly,
and local tx are slower. or stuff is locked rarely, and global tx are slower.
good UX: user don't care about local vs global.
worst-case local = 1 local round-trip if not locked / 1 global round-trip if locked. ~100ms / ~1s
worst-case global = waiting for the lock + 1 global operation. ~10s

ii contracts -> i contracts
i+ii?

iii
a global can happen at anytime, but if it touches local data,
it must lock, lock takes 2 blocks, op + unlock takes 2 blocks.
worst-case local = 2 global round-trip + 2 blocks

**conclusion: start with i, later move to i+ii, lazyness yay**

95% = local
5% = global

1 block every 10 is global
1 block = 500 ms
lock time = 2 block

5% tx need to wait for 10seconds

i

Smart-contracts that touch state that is only in X, or in X and Y

If smart-contract only touches state in X, X's consensus is enough
If smart-contract touches state in multiple places, it's global consensus

local

A is in X
B is in X

1.  current block producer aggregates transactions (between people)
2.  at a given time, put them in a block, signs the block, broadcast it
3.  everyone else signs the block, broadcast signatures
4.  go back to step 1
    4 bis. if people don't get a signed block after a timeout, broadcasts message stating so
    5 bis. if everyone gets 2/3rd stating so, move to the next block producer

global

A is in X
B is in Y

1.  current block producer aggregates transactions (between people)

\*/

/\*

\*/

/\*

# HTTP

## Users

POST /transaction
POST /freeze

## Tezos Daemon

POST /last-updates
-> sends a list of updates in Tezos + number of the latest one.

## Nodes

POST /broadcast-signed-block
POST /broadcast-transaction

\*/

/\*

timeout when a block doesn't send, bootstrapped

A -> B -> C -> D
A is the current producer
A sends a block every 3 seconds.
0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15
^ - - - - - ^ - - - - - - - - - - - A
B1 B2 - -- - -- - C

0 - A
0.5s - B
0.6s - C

start counting when you receive the last block

\*/

/_
a block is only considered received AFTER it is json parsed,
after the JSON is parsed all the requests are sync,
otherwise it needs to lock the state
_/

/\*

how to mitigate a 2/3 attack:

to add an remove a validator, it expects a master chain finality
if 1/3 of the chain is corrupted at same time, then it stales, no money loss
if a validator stops,

if someone votes 2 times, it gets slashed
if 2/3 votes 2 times, then all the validators that voted 2/3 loose their money and
everyone that did a double spend looses all their money
if 2/3 of the chain is corrupted

---

## Out of order execution

**caution**

Because we have multicore we can compute in parallel, and assume that a block will be signed, so applying it in parallel and switching if it actually get's signed.

The same is valid for the block producer, who can produce and apply a new block because if he is not doing anything wrong, it's quite likely to be the signed block.

This also allows to reduce the latency as the block is sent faster, care on the interaction with the flag mechanism.

## Validators

Because the validators are handled off chain we need a way for new validators, to ensure that at any time everyone agrees on the validators, the list can only change when a state root is changed.

But we need a way to proof to the new validator what is the current list of validators, to achieve this we have two options

- Allow nodes who download a snapshot to self validate it by making so that all the blocks changing validators must be self signed, meaning that it contains the needed signatures at the time when it was merged.

**this probably is bad because people can lie and change the history with this, imagine 2/3 of old validators are upset, they can make themselves the new validators**

Disadvantages is that we need to request signatures, which is something never needed in the current protocol

- The node asks for a snapshot and keep listening to blocks until block X gets accepted on tezos and because he knows all blocks that happened since X he is already in sync.

Disadvantages needs to wait stuff happening on the main chain, which means waiting a finality to happens on tezos, not fun ;/

## Hashing


TODO: how to make the contract upgradable

H(consensus) = H(
  H(block_height) +
  last_block_hash +
  previous_state_hash +
  H(validators)
)
TODO: maybe each ledger cell should have more values?
      Is this worth in performance and gas cost on chain?
H(ledger) =
  if empty then
    H(nil)
  else
    H(H(left) + H(right))
)
H(data) = H(ledger)
H(protocol) = H(consensus) + H(data)

### Goals

H(consensus)

H(block_height) prevents reply attacks, as a block height must always increase
last_block_hash guarantees that this state was created by this chain
previous_state_hash mostly guarantees the initial state hash,
H(validators) defines who are the validators when this state hash was commited
              important, a node need to follow the chain until it sees a known
              block hash being merged on tezos, this ensures that we know who
              are the current validators at any point without needing to rely
              on asking for old signatures.

Note that we don't enforce on mainchain the previous_state_hash,
this allows to skip state hashes and also allows to change the past

H(data)

This is the data that will be validated by the data contract

H(ledger) the goal here is to make so that we don't need to keep the data on
          tezos but when needed any user can get the needed hashes and submit
          a proof to tezos that during state hash X he holds funds

## Withdraw

Problems the state hash is commited async, so we cannot simply say "you have a state epoch to withdraw your funds" as you will be in a racing condition against the sidechain. 

### Direct Withdraw

Collect all the ledger hashes needed
  -> send a tezos op

Pros:
  Simple, can be used even if the sidechain stales
Cons:
  Racing condition against the sidechain

It may be possible to ensure a withdraw window
Ideas:
  Ensure no operation in the recent sidechain blocks 
get all hashes -> send tezos op -> withdraw

Scheduled:

send tezos op -> request_funds_contract -> sidechain see and includes on root
update, after updating the hash the consensus contract triggers subsequenct
operations on the same block, ensuring that you can always withdraw.
flow can be, you do an operation on tezos asking for 10 funds,

Same will probably be valid when we start doing smart contract

## Optimistic rollups funds

It may be desirable to integrate optimistic rollups logic so that we can
validate funds transfer on sidechain on the mainchain if needed. This
would allow an additional level of confidence as even if all nodes
decide to break the sidechain guarantees users will not loose funds.

This would freeze the consensus contract in the non disputed state,
and because the contract is frozen all balances are also considered
frozen, so users can just do direct withdraw.
