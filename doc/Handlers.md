# Handlers

Each Deku-node exposes several HTTP/TCP endpoints to interact with the chain.
They are available within [deku_node.ml](../src/bin/deku_node.ml)
Here are listed each of them, with a `curl` example command to play with them.

Since we use RPC, all these endpoints are POST (only verb available over RPC).

## Prerequisites

In order to be able to play the following `curl` examples, you must have

- run `docker compose upd -d`
- run `./sandbox.sh setup`
- run `./start.sh`

## `/append-block-and-signature`

### Purpose

Check if the provided block is valid, where `valid` means:

- Height of the provided block is higher that the current height
- Each operation of this block is properly signed

### Expected data

- A [block](../src/protocol/block.mli#L2)
- The corresponding [signature](../src/protocol/protocol_signature.ml#L3)

### curl example

```shell script
$
```

## `/append-signature`

### Purpose

Add the provided signature to the provided hash.

### Expected data

- An hash of type `BLAKE2B.t`
- The corresponding [signature](../src/protocol/protocol_signature.ml#L3)

### curl example

```shell script
$
```

## `/block-by-hash`

### Purpose

Retrieve block corresponding to the provided hash.

### Expected data

An hash

### Returned data

JSON of the corresponding [block](../src/protocol/block.mli#L2).

### curl example

```shell script
$
```

## `/block-level`

### Purpose

Retrieve the current height of the chain

### Expected data

None.

### Returneed data

The current height of type `int64`

## `/protocol-snapshot`

### Purpose

Retrieve the snapshot of the protocol.

### Expeted data

None.

### Returned data

- The current [snapshot](../src/node/snapshots.mli#L3)
- The additional blocks (which are not yet added to the chain)
- The last added block
- The signature of the last added block

### curl example

```shell script
$
```

## `/request-nonce`

### Purpose

??

### Expected data

A URI?

### Returned data

The nonce of type `BLAKE2B.t`

## `/register-uri`

### Purpose

??Add the provided URI as a new validator??

### Expected data

- A URI
- The corresponding signature

### curl example

```shell script
$
```

## `/user-operation-gossip`

### Purpose

Add a user operation to the pending operations.
If the operation does not already exist, it is broadcasted to the gossip network

### Expected data

A [`core_user.t`](../src/protocol/protocol_operation.mli#L14).

### curl example

```shell script
$
```

## `/consensus-operation-gossip`

### Purpose

Like a user operation, but from consensus: add a consensus operation to the pending operations.

### Expected data

- The [consensus operation](../src/protocol//protocol_operation.mli#L3) to add
- The corresponding signature

### curl example

```shell script
$
```

## `/trusted-validators-membership`

### Purpose

Manage the trusted validators. It is possible to `add` or `remove` a validator.

### Expected data

- An [`action`](../src/node/networking.ml#L135)
- The corresponding [`key_hash`](../src/crypto/key_hash.mli#L#)

### Returned data

- The signature
- The provided payload (action + key_hash)

### curl example

```shell script
$
```

## `/withdraw-proof`

### Purpose

????

### Expected data

The hash of the operation to withdraw?

### Returned data

????

### curl example

```shell script
$
```

## `/ticket-balance`

### Purpose

????

### Expected data

- An address of type `Key_hash.t`
- A ticket id

### Returned data

The amount

### curl example

```shell script
$
```
