# Design for Async Hashing

## Problem

State root hashes are unique identifiers by which you can download a state (in
the form of a state snapshot) from any node in the network. When the state root
changes, it's like genesis shifts from the original genesis block to the new
block. So, to get the current state, instead of applying all the blocks from
genesis to now, all you need to do is
[observe the current state root hash published to Tezos](https://github.com/marigold-dev/deku/blob/main/tezos_interop/consensus.mligo#L13),
ask a node for the state snapshot corresponding to it, and apply the blocks
between now then and now.

The faster we update the state root hash, the less blocks you need to go from
not in sync to in sync.

Currently state root hashing is done synchronously
[when producing](https://github.com/marigold-dev/deku/blob/main/protocol/block.re#L178)
and
[receiving a block](https://github.com/marigold-dev/deku/blob/main/protocol/protocol.re#L176-L177).

We do not update the state root hash every block - this would be expensive
without much benefit. Instead, we batch the operation,
[updating the state root hash once every 60 seconds](https://github.com/marigold-dev/deku/blob/main/protocol/block.re#L159-L172).

While the hashing happens on a delay, it happens synchronously, presenting a big
bottleneck on each state root epoch. We want to change this to happen
asynchronously.

To state it concisely, we need an algorithm for block producers and validators
such that:

1. If an honest node hashes a particular state, every other node will either
  eventually hash that same state or has already hashed that state.
2. The block producer is forced to produce state root hash updates on time.
  - 2a. Validators reject blocks with state root hash changes that occur too
    early.
  - 2b. Validators reject blocks with a state root hash that is too old.
3 The state root hash can only advance forward - that is, when a validator sees
  a block with state root hash h, and then a subsequent block with state root
  hash h', the validator rejects any subsequent blocks with state root hash h.
4. Honest validators that are in sync and have sufficient hashing power always
  have the next state root hash before it is needed (i.e, hashing is no longer a
  bottleneck).
5. Honest validators that are behind in hashing mark themselves as out of sync
  and continue to follow but don't sign blocks.
6. Honest validators can get back in sync when finished hashing (assuming they
  were in sync before they fell behind).

## Definitions

_state root epoch (EPOCH)_: interval (in blocks) between the events of current
state root hash update (this accounts for any delays in hashing/network/etc.)

_state root epoch min timeout (MIN_TIMEOUT)_: The interval after a state root
epoch begins before which validators will not sign a blocks in that epoch.

_state root epoch max timeout (MAX_TIMEOUT)_: The interval after a state root
epoch begins after which validators will no longer sign blocks in that epoch.

## Solution/Design

We'll consider the new design separately for block producers and validators.

### Block Producer

In the current design, a timeout causes the block producer to hash a new state
and update the current state root hash synchronously before producing the next
block. By definition, this creates a new EPOCH. This means that the hash of
state at the start of the new EPOCH is that is the one used for the whole EPOCH.
This looks something like the following:
![old design](https://user-images.githubusercontent.com/25231383/134707924-44ad9ed0-7f1b-41a4-9c80-363016945146.png)

In the proposed design, the causation is reversed: The start of a new EPOCH
triggers the block producer to start hashing the current state asynchronously.
Additionally, a timeout of length MIN_TIMEOUT is started. When either a) the
hash is complete or b) the timeout finishes (which ever takes longer), the
completed hash becomes the current state root hash, triggering a new EPOCH,
repeating the cycle. This results in a 1 EPOCH delay between when a state root
hash is produced and when the hash is used, as depicted below:
![current design](https://user-images.githubusercontent.com/25231383/134708767-17cb19c0-9fda-4011-b0fb-2cf0e488d2d1.png)

By waiting until at least MIN_TIMEOUT until using an updated state root hash,
the block producer ensures his blocks will be accepted by validators constrained
by requirement 1a.

### Validators

In the current design, state hashing for validators was simple: at the start of
a new EPOCH, hash the current state and update the current state root hash to
that hash. A new EPOCH begins by definitions when a block with a different state
root hash than the current one is applied.

In the proposed design, the trigger is the same: at the start of a new EPOCH,
start hashing the current state asynchronously. However, we relax the constraint
that a block's state root hash must be known before it can be applied. Instead a
validator may apply a block with an unknown state root hash as long as it meets
the other criteria (such as having the required majority of signatures and an
appropriate block height, etc.).

A consequence is that it is impossible to guarantee that:

- a) the hash of a particular EPOCH will be finished before the next EPOCH
  begins
- b) a block's state root hash is known (i.e the validator re-derives the state
  root hash by hashing its own state).

It's therefore possible for a validator to become out of sync w.r.t. to state
root hashes (other parts of being in sync are not addressed in this design). To
allow for this in a way that achieves requirements 1-5, we introduce the
following design state, actions, and rules:

We introduce the following state, actions, and rules:

#### State

current_state_root_hash : hash - the current state root hash (this is already
implemented in the protocol state).

done : Set((block_height, hash)) - the set of hashes that a validator has
finished hashing but has not yet witnessed on incoming blocks. Additionally,
each hash is paired with the block height to which it corresponds (i.e the block
height of the state that was hashed).

epoch_start_time : a time stamp of when the current EPOCH started.

pending_but_applied: Set(hash) - a set of hashes that the validator has applied
but cannot verify. If this set is non-empty, the node cannot be in sync.

candidate_blocks : Map of { [hash]: Set(Block.t) } - a map from state root
hashes to sets of blocks can be signed if the respective hash is ever produced
via hashing.

#### Actions

The following actions can occur to change the nodes state w.r.t to the state
root hash:

- The validator can receive a block
- The validator can apply a block
- A new EPOCH can begin (this is triggered by the special case of applying a
  block where the state root hash is different than the current one)
- A hash that was started asynchronously can finish

#### Rules

Written in rule-based pseudo-code.

Let there be the following functions:

- MIN_TIMEOUT = () => Unix.time() - EPOCH_INITIAL_TIME >= MIN_TIMEOUT_NUMBER

- MAX_TIMEOUT = () => Unix.time() - EPOCH_INITIAL_TIME >= MAX_TIMEOUT_NUMBER

The following rules describe the validators' state root hash algorithm:

- when receiving a block block : Block.t

  - when block.state_root_hash = current_state_root_hash && ! MAX_TIMEOUT
    - Then you may sign the bock
  - when block.state_root_hash != current_state_root_hash && MIN_TIMEOUT &&
    (block_height_of_start_of_epoch, block.state_root_hash) \in done
    - Then you may sign the block.
  - when block.state_root_hash != current_state_root_hash && MIN_TIMEOUT &&
    block.state_root_hash \not_in done
    - Then you are not allowed to sign the block
    - Add the {[hash]: block} pair to the candidate_blocks map.

- when applying a block block : Block.t
  - when block.state_root_hash != current_state_root_hash
  - Start a new EPOCH
  - current_state_root_hash := block.state_root_hash
  - when block.state_root_hash \notin done
    - Add block.state_root_hash to pending_but_applied.
    - Mark the node as out of sync.
  - when in sync
    - remove any hashes with a block height lower than the current block height
      from the set done.
    - remove any hashes with a block height lower than the current block height
      from the map candidate_blocks candidate_blocks
    - remove any hashes with a block height lower than the current block height
      from the set pending_but_applied
- when EPOCH starts
  - EPOCH_INITIAL_TIME := Unix.time()
  - start hashing current state with the current block height
- when you finish hashing hash with block height bh : block_height
  - done := {(bh, hash)} + done
  - when hash \in candidate_blocks
  - try_to_apply_all_blocks(candidate_blocks[hash])
  - SPECIAL CASE: if the hash is in pending_but_applied and before the hash
    being added to pending_but_applied we're in sync and no other change put the
    node out-of-sync since the hash was added to pending (TODO: clarify this
    notion)
    - switch the node to being in sync

## Considerations and Gotcha's

**How do validators agree when an EPOCH starts?**

For honest validators in a given state and EPOCH, if the validators that are in
sync, there are only two possibilities:

- They eventually apply a block with a new state root.
- The block producer fails to produce a block before MAX_TIMEOUT which receives
  enough signatures to apply. In the first case, by definition of "in sync", all
  the honest in-sync nodes will agree on the start of the new EPOCH.

In the second case (TODO: work out informal proof why the second case is ok).

**State should not grow forever**

Good theorems to prove would be that it is impossible for the cardinalities of
`done`, `candidate_blocks`, and `pending_but_applied` to grow forever.

This can be formalized roughly as "every hash/block added to the set/map is
eventually removed".

This could probably be more easily model-checked than proven.

**How to limit parallelism?**

We may reach a state where there is too many state root hashes on the queue in
parallel and that will slow down the node making so that the queue grows to
infinite.
