---
id: crypto-reference
title: Crypto
description: Cryptographic operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
function blake2b : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val blake2b : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let blake2b: bytes => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let blake2b: (b: bytes) => bytes
</SyntaxTitle>

Runs the [blake2b hash algorithm](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2)
over the given `bytes` data and returns a `bytes` representing the hash.



<Syntax syntax="pascaligo">

```pascaligo
function hasherman_blake (const s: bytes) : bytes is Crypto.blake2b(s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let hasherman_blake (s: bytes) : bytes = Crypto.blake2b s
```



</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let hasherman_blake = (s: bytes) => Crypto.blake2b(s);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let hasherman_blake = (s: bytes):bytes => Crypto.blake2b(s);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function sha256 : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sha256 : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sha256: bytes => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sha256: (b: bytes) => bytes
</SyntaxTitle>

Runs the [sha256 hash algorithm](https://en.wikipedia.org/wiki/SHA-2) over the given
`bytes` data and returns a `bytes` representing the hash.



<Syntax syntax="pascaligo">

```pascaligo
function hasherman (const s : bytes) : bytes is Crypto.sha256(s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let hasherman (s : bytes) : bytes =
   Crypto.sha256 s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let hasherman = (s: bytes): bytes => Crypto.sha256(s);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let hasherman = (s: bytes): bytes => Crypto.sha256(s);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function sha512 : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sha512 : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sha512: bytes => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sha512: (b: bytes) => bytes
</SyntaxTitle>

Runs the [sha512 hash algorithm](https://en.wikipedia.org/wiki/SHA-2) over the given
`bytes` data and returns a `bytes` representing the hash.



<Syntax syntax="pascaligo">

```pascaligo
function hasherman512 (const s: bytes) : bytes is Crypto.sha512(s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let hasherman512 (s: bytes) : bytes = Crypto.sha512 s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let hasherman512 = (s: bytes) => Crypto.sha512(s);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let hasherman512 = (s: bytes): bytes => Crypto.sha512(s);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function sha3 : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sha3 : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sha3: bytes => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sha3: (b: bytes) => bytes
</SyntaxTitle>

Runs the [sha3 hash algorithm](https://en.wikipedia.org/wiki/SHA-3) over the given
`bytes` data and returns a `bytes` representing the hash.



<Syntax syntax="pascaligo">

```pascaligo
function hasherman3 (const s: bytes) : bytes is Crypto.sha3(s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let hasherman3 (s: bytes) : bytes = Crypto.sha3 s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let hasherman3 = (s: bytes) => Crypto.sha3(s);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let hasherman3 = (s: bytes): bytes => Crypto.sha3(s);
```

</Syntax>
<SyntaxTitle syntax="pascaligo">
function keccak : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val keccak : bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let keccak: bytes => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let keccak: (b: bytes) => bytes
</SyntaxTitle>

Runs the [keccak](https://en.wikipedia.org/wiki/keccak) over the given
`bytes` data and returns a `bytes` representing the hash.

<Syntax syntax="pascaligo">

```pascaligo
function hasherman_keccak (const s: bytes) : bytes is Crypto.keccak(s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let hasherman_keccak (s: bytes) : bytes = Crypto.keccak s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let hasherman_keccak = (s: bytes) => Crypto.keccak(s);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let hasherman_keccak = (s: bytes): bytes => Crypto.keccak(s);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function hash_key : key -> key_hash
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val hash_key : key -> key_hash
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let hash_key: key => key_hash
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let hash_key: (k: key) => key_hash
</SyntaxTitle>

Hashes a key for easy comparison and storage.



<Syntax syntax="pascaligo">

```pascaligo
function check_hash_key (const kh1 : key_hash; const k2 : key) : bool * key_hash is block {
  var ret : bool := False ;
  var kh2 : key_hash := Crypto.hash_key(k2) ;
  if kh1 = kh2 then ret := True else skip; 
} with (ret, kh2)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let check_hash_key (kh1, k2: key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2 in
  if kh1 = kh2
  then (true, kh2)
  else (false, kh2)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let check_hash_key = ((kh1, k2): (key_hash, key)) : (bool, key_hash) => {
  let kh2 : key_hash = Crypto.hash_key(k2);
  if (kh1 == kh2) {
    (true, kh2);
  }
  else {
    (false, kh2);
  }
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let check_hash_key = ([kh1, k2]: [key_hash, key]) : [bool, key_hash] => {
  let kh2 : key_hash = Crypto.hash_key(k2);
  if (kh1 == kh2) {
    return [true, kh2];
  }
  else {
    return [false, kh2];
  };
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function check : key -> signature -> bytes -> bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val check : key -> signature -> bytes -> bool
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let check: (key, signature, bytes) => bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let check: (k: key, s: signature, b: bytes) => bool
</SyntaxTitle>

Check that a message has been signed by a particular key.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is because that would require storing a private key on chain, at which point it isn't very private anymore.



<Syntax syntax="pascaligo">

```pascaligo
function check_signature
    (const pk: key;
     const signed: signature;
     const msg: bytes) : bool
  is Crypto.check(pk, signed, msg)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let check_signature (pk, signed, msg: key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let check_signature = ((pk, signed, msg): (key, signature, bytes)) : bool => {
  Crypto.check(pk, signed, msg);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let check_signature = ([pk, signed, msg]: [key, signature, bytes]) : bool => {
  return Crypto.check(pk, signed, msg);
};
```

</Syntax>

