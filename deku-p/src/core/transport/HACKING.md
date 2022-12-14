# Deku Transport Protocol

This document intends on documenting the transport protocol used by Deku. It is currently built on top of TCP, but in the future with some extensions it should be possible to move it to UDP.

## Rationale

The transport layer needs some form of checksum to ensure that any problems in transport did not change the content of the message and it also needs some form of authentication to ensure that the message was sent by the expected peer.

If instead of checksum we used a cryptographic hash, this computing could then be used internally on deku reducing the total amount of computing required.

## Protocol Frame

hash = blake2b (control ^ blake2b (data));
data size < 128mb.

| length    | header       | frame payload                |
| --------- | ------------ | ---------------------------- |
| int32 > 0 | char[length] | length : int32, char[length] |

## Fields

### Frame Length

Used to preallocate a buffer.

```rust
length = sizeof(hash) + sizeof(control_length) + control_length + sizeof(payload_length) + payload_length;
```

### Frame Hash

Used to detect corrupted messages, can also be used to avoid decoding the same message many times.

```rust
hash = blake2b ((blake2b control) + (blake2b payload))1;
```

Because of this structure the `blake2b payload` value can be stored and be reused later.

### Control

| signature | hash     | author   | payload_length | tag   | level  |
| --------- | -------- | -------- | -------------- | ----- | ------ |
| char[64]  | char[32] | char[20] | uint32 >= 0    | uint8 | uint56 |

```rust
hash = blake2b (author + payload_length + tag + level + (blake2b payload))
```

Used to communicate "control information", such as which level the message was produced and which kind of message it is. This is used both by middlewares such as deku-switch and by deku-node itself to tag messages, hopefully in the future it can be used for scheduling.

Control length zero is meaningless and reserved, if for some unfortunate reason we need to extend this protocol without a breaking change, it could be used.

### Future

Yes, I almost reinvented HMAC, we should probably just use proper HMAC. Or use blake2b keying mechanism.

### Properties

Memory friendly
Data integrity
Authenticated
Priority
Timestamping

### Streaming

Switch -> Node -> Deserialize
Node -> Serialize -> Switch

### Footer

Ideally the header should be a footer, so that we can start sending the message before having the hash of the entire message. This would allow to stream serialization.

This only works for trusted connections, as otherwise we would need to hash all messages, including duplicated messages.

Even without trusted connections it could work, we just need another id besides of the hash, for blocks it could be level + author, then we confirm the hash with everyone else if author produces two blocks then we exchange it again, but this time using the hash as id.

## Frame Header

| length |
| ------ |
| uint16 |

wait -> add 0 -> add 1 -> add 2 -> add 0
