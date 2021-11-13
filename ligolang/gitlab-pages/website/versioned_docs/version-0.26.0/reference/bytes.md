---
id: bytes-reference
title: Bytes
description: Operations on bytes
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
function concat : bytes -> bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val concat : bytes -> bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let concat: (bytes, bytes) => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let concat: (a: bytes, b: bytes) => bytes
</SyntaxTitle>


Concatenate together two `bytes` arguments and return the result.

<Syntax syntax="pascaligo">

```pascaligo
function concat_op (const s : bytes) : bytes is Bytes.concat(s , 0x7070)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let concat_op (s : bytes) : bytes =
   Bytes.concat s 0x7070
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let concat_op = (s: bytes): bytes => Bytes.concat(s, 0x7070);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let concat_op = (s: bytes): bytes => Bytes.concat(s, 0x7070);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function sub : nat -> nat -> bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sub : nat -> nat -> bytes -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sub : (nat, nat, bytes) => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sub : (start: nat, length: nat, input: bytes) => bytes
</SyntaxTitle>

Extract bytes from `start` to `length`. For example if you gave the 
input "ff7a7aff" to the following function:

<Syntax syntax="pascaligo">

```pascaligo
function slice_op (const s : bytes) : bytes is Bytes.sub(1n , 2n , s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let slice_op (s : bytes) : bytes = Bytes.sub 1n 2n s
```

</Syntax>
<Syntax syntax="reasonligo">

```
let slice_op = (s: bytes): bytes => Bytes.sub(1n, 2n, s);
```

</Syntax>
<Syntax syntax="jsligo">

```
let slice_op = (s: bytes): bytes => Bytes.sub(1 as nat, 2 as nat, s);
```

</Syntax>

It would return "7a7a".

<SyntaxTitle syntax="pascaligo">
function pack : 'a -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val pack : 'a -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let pack : 'a => bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let pack : (data: &apos;a) => bytes
</SyntaxTitle>

Converts Michelson data structures to a binary format for serialisation.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.



<Syntax syntax="pascaligo">

```pascaligo
function id_string (const p : string) : option(string) is block {
  const packed : bytes = Bytes.pack(p) ;
} with (Bytes.unpack(packed): option(string))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let id_string = (p: string) : option(string) => {
  let packed : bytes = Bytes.pack(p);
  ((Bytes.unpack(packed)): option(string));
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=pack
let id_string = (p: string) : option<string> => {
  let packed : bytes = Bytes.pack(p);
  return (Bytes.unpack(packed) as option<string>);
};
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function unpack : bytes -> option 'a
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val unpack : bytes -> 'a option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let unpack: bytes => option('a)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let unpack: (serialized_data: bytes) => option&lt;&apos;a&gt;
</SyntaxTitle>

Reverses the result of using `pack` on data. 

As the conversion might fail an option type is returned.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.



<Syntax syntax="pascaligo">

```pascaligo
function id_string (const p : string) : option(string) is block {
  const packed : bytes = Bytes.pack(p) ;
} with (Bytes.unpack(packed): option(string))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let id_string = (p: string) : option(string) => {
  let packed : bytes = Bytes.pack(p);
  ((Bytes.unpack(packed)): option(string));
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let id_string = (p: string) : option<string> => {
  let packed : bytes = Bytes.pack(p);
  return (Bytes.unpack(packed) as option<string>);
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function length : bytes -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val length : bytes -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let length: bytes => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let length: (b: bytes) => nat
</SyntaxTitle>
