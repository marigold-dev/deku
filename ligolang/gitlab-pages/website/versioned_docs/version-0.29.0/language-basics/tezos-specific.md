---
id: tezos-specific
title: Tezos Domain-Specific Operations
---

import Syntax from '@theme/Syntax';

LIGO is a programming language for writing Tezos smart contracts. It
would be a little odd if it did not have any Tezos specific
functions. This page will tell you about them.

## Pack and Unpack

As Michelson provides the `PACK` and `UNPACK` instructions for data
serialisation, so does LIGO with `Bytes.pack` and `Bytes.unpack`.  The
former serialises Michelson data structures into a binary format, and
the latter reverses that transformation. Unpacking may fail, so the
return type of `Byte.unpack` is an option that needs to be annotated.

> ⚠️ `PACK` and `UNPACK` are Michelson instructions that are intended
> to be used by people that really know what they are doing. There are
> several risks and failure cases, such as unpacking a lambda from an
> untrusted source or casting the result to the wrong type. Do not use
> the corresponding LIGO functions without doing your homework first.



<Syntax syntax="pascaligo">

```pascaligo group=a
function id_string (const p : string) : option (string) is block {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (string))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let id_string (p : string) : string option =
  let packed: bytes = Bytes.pack p in
  (Bytes.unpack packed : string option)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=a
let id_string = (p : string) : option (string) => {
  let packed : bytes = Bytes.pack (p);
  (Bytes.unpack(packed) : option (string));
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=a
let id_string = (p: string): option<string> => {
  let packed: bytes = Bytes.pack(p);
  return (Bytes.unpack(packed) as option<string>);
};
```

</Syntax>


## Hashing Keys

It is often desirable to hash a public key. In Michelson, certain data
structures such as maps will not allow the use of the `key` type. Even
if this were not the case, hashes are much smaller than keys, and
storage on blockchains comes at a cost premium. You can hash keys with
a predefined functions returning a value of type `key_hash`.



<Syntax syntax="pascaligo">

```pascaligo group=b
function check_hash_key (const kh1 : key_hash; const k2 : key) : bool * key_hash is
  block {
    var ret : bool := False;
    var kh2 : key_hash := Crypto.hash_key (k2);
    if kh1 = kh2 then ret := True else skip
  } with (ret, kh2)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let check_hash_key (kh1, k2 : key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2 in 
  (kh1 = kh2), kh2
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let check_hash_key = ((kh1, k2) : (key_hash, key)) : (bool, key_hash) => {
  let kh2 : key_hash = Crypto.hash_key (k2);
  ((kh1 == kh2), kh2);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let check_hash_key = ([kh1, k2]: [key_hash, key]): [bool, key_hash] => {
  let kh2: key_hash = Crypto.hash_key(k2);
  return [(kh1 == kh2), kh2];
};
```

</Syntax>


## Checking Signatures

Sometimes a contract will want to check that a message has been signed
by a particular key. For example, a point-of-sale system might want a
customer to sign a transaction so it can be processed
asynchronously. You can do this in LIGO using the `key` and
`signature` types.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is
> because that would require storing a private key on chain, at which
> point it is not... private anymore.



<Syntax syntax="pascaligo">

```pascaligo group=c
function check_signature
    (const pk     : key;
     const signed : signature;
     const msg    : bytes) : bool
  is Crypto.check (pk, signed, msg)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let check_signature =
  ((pk, signed, msg) : (key, signature, bytes)) : bool =>
  Crypto.check (pk, signed, msg);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let check_signature =
  ([pk, signed, msg]: [key, signature, bytes]): bool =>
  Crypto.check(pk, signed, msg);
```

</Syntax>


## Contract's Own Address

Often you want to get the address of the contract being executed. You
can do it with `Tezos.self_address`.

> ⚠️ Due to limitations in Michelson, `Tezos.self_address` in a
> contract is only allowed at the top-level. Using it in an embedded
> function will cause an error.



<Syntax syntax="pascaligo">

```pascaligo group=d
const current_addr : address = Tezos.self_address
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=d
let current_addr : address = Tezos.self_address
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=d
let current_addr : address = Tezos.self_address;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=d
let current_addr: address = Tezos.self_address;
```

</Syntax>

## Origination of a contract

`Tezos.create_contract` allows you to originate a contract given its code, delegate (if any), initial balance and initial storage.
The return value is a pair of type `(operation * address)`.

> ⚠️ Due to limitations in Michelson, `Tezos.create_contract` first argument
> must be inlined and must not contain references to free variables

<Syntax syntax="pascaligo">

```pascaligo group=e
const origination : operation * address = Tezos.create_contract (
  function (const p : nat; const s : string): list(operation) * string is ((nil : list(operation)), s),
  (None : option(key_hash)),
  3tz,
  "initial_storage")
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=e
let origination : operation * address = Tezos.create_contract
  (fun (p, s : nat * string) -> (([] : operation list), s))
  (None: key_hash option) 
  3tz 
  "initial_storage"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=e
let origination : (operation, address) = Tezos.create_contract (
  ((p, s) : (nat,string)) : (list(operation),string) => (([] : list(operation)), s),
  None: option(key_hash),
  3tz,
  "initial_storage")
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=e
let origination : [operation, address] = Tezos.create_contract (
  ([p, s]: [nat,string]): [list<operation>, string] => [(list([]) as list<operation>), s],
  None() as option<key_hash>,
  3 as tez,
  "initial_storage");
```

</Syntax>

