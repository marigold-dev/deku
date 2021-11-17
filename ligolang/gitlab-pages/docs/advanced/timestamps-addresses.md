---
id: timestamps-addresses
title: Timestamps, Addresses
---

import Syntax from '@theme/Syntax';

## Timestamps

LIGO features timestamps, as Michelson does, while bakers baking the
block (including the transaction in a block) are responsible for
providing the given current timestamp for the contract.

### Starting time of the current block

You can obtain the starting time of the current block using the 
built-in `Tezos.now`. This timestamp does not change during the execution 
of the contract. Please be aware that it is up to the baker to set the
current timestamp value.

<Syntax syntax="pascaligo">

```pascaligo group=a
const today : timestamp = Tezos.now
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let today : timestamp = Tezos.now
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=a
let today : timestamp = Tezos.now;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=a
let today: timestamp = Tezos.now;
```

</Syntax>


> When running code, the LIGO CLI option `--now`
> allows you to control what `Tezos.now` returns.

### Timestamp Arithmetics

In LIGO, timestamps can be added to integers, allowing you to set time
constraints on your smart contracts. Consider the following scenarios.

#### In 24 hours


<Syntax syntax="pascaligo">

```pascaligo group=b
const today : timestamp = Tezos.now
const one_day : int = 86_400
const in_24_hrs : timestamp = today + one_day
const some_date : timestamp = ("2000-01-01T10:10:10Z" : timestamp)
const one_day_later : timestamp = some_date + one_day
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let today : timestamp = Tezos.now
let one_day : int = 86_400
let in_24_hrs : timestamp = today + one_day
let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later : timestamp = some_date + one_day
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let today : timestamp = Tezos.now;
let one_day : int = 86_400;
let in_24_hrs : timestamp = today + one_day;
let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp);
let one_day_later : timestamp = some_date + one_day;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let today: timestamp = Tezos.now;
let one_day: int = 86_400;
let in_24_hrs: timestamp = today + one_day;
let some_date: timestamp = "2000-01-01t10:10:10Z" as timestamp;
let one_day_later: timestamp = some_date + one_day;
```

</Syntax>


#### 24 hours Ago


<Syntax syntax="pascaligo">

```pascaligo group=c
const today : timestamp = Tezos.now
const one_day : int = 86400
const in_24_hrs : timestamp = today - one_day
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let today : timestamp = Tezos.now
let one_day : int = 86400
let in_24_hrs : timestamp = today - one_day
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let today : timestamp = Tezos.now;
let one_day : int = 86400;
let in_24_hrs : timestamp = today - one_day;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let today: timestamp = Tezos.now;
let one_day: int = 86400;
let in_24_hrs: timestamp = today - one_day;
```

</Syntax>


### Comparing Timestamps

You can compare timestamps using the same comparison operators
applying to numbers.


<Syntax syntax="pascaligo">

```pascaligo group=c
const not_tommorow : bool = (Tezos.now = in_24_hrs)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let not_tomorrow : bool = (Tezos.now = in_24_hrs)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let not_tomorrow : bool = (Tezos.now == in_24_hrs);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let not_tomorrow: bool = (Tezos.now == in_24_hrs);
```

</Syntax>


## Addresses

The `address` type in LIGO denotes Tezos addresses (tz1, tz2, tz3,
KT1, ...). Currently, addresses are created by casting a string to the
`address` type. Beware of failures if the address is invalid. Consider
the following examples.


<Syntax syntax="pascaligo">

```pascaligo group=d
const my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=d
let my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=d
let my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=d
let my_account: address =
  "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;
```

</Syntax>

## Signatures

The `signature` type in LIGO datatype is used for Tezos signatures
(edsig, spsig). Signatures are created by casting a string. Beware of
failure if the signature is invalid.

Here is how you can define a signature:


<Syntax syntax="pascaligo">

```pascaligo group=e
const my_sig : signature =
  ("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
  signature)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=e
let my_sig : signature =
   ("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
   signature)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=e
let my_sig : signature =
("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
signature);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=e
let my_sig: signature =
"edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" as
signature;
```

</Syntax>


## Keys

The `key` type in LIGO is used for Tezos public keys. Do not confuse
them with map keys. Keys are made by casting strings. Beware of
failure if the key is invalid.

Here is how you can define a key.


<Syntax syntax="pascaligo">

```pascaligo group=f
const my_key : key =
("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=f
let my_key : key =
  ("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=f
let my_key : key =
  ("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=f
let my_key : key =
  "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" as key;
```

</Syntax>

