---
id: strings-bytes
title: Strings & Bytes
---

import Syntax from '@theme/Syntax';

## Strings

Strings are defined using the built-in `string` type like this:


<Syntax syntax="pascaligo">

```
const a : string = "Hello Alice"
```

</Syntax>
<Syntax syntax="cameligo">

```
let a : string = "Hello Alice"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let a : string = "Hello Alice";
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let a: string = "Hello Alice";
```

or with single quotes:

```jsligo
let a_: string = 'Hello Alice';
```

</Syntax>



### Concatenating Strings


<Syntax syntax="pascaligo">

Strings can be concatenated using the `^` operator.

```pascaligo group=a
const name : string = "Alice"
const greeting : string = "Hello"
const full_greeting : string = greeting ^ " " ^ name
```

</Syntax>
<Syntax syntax="cameligo">

Strings can be concatenated using the `^` operator.

```cameligo group=a
let name : string = "Alice"
let greeting : string = "Hello"
let full_greeting : string = greeting ^ " " ^ name
```

</Syntax>
<Syntax syntax="reasonligo">

Strings can be concatenated using the `++` operator.

```reasonligo group=a
let name : string = "Alice";
let greeting : string = "Hello";
let full_greeting : string = greeting ++ " " ++ name;
```

</Syntax>
<Syntax syntax="jsligo">

Strings can be concatenated using the `+` operator.

```jsligo group=a
let name: string = "Alice";
let greeting: string = "Hello";
let full_greeting: string = greeting + " " + name;
```

</Syntax>



### Extracting Substrings

Substrings can be extracted using the predefined function
`String.sub`. The first character has index 0 and the interval of
indices for the substring has inclusive bounds.

<Syntax syntax="pascaligo">

```pascaligo group=b
const name  : string = "Alice"
const slice : string = String.sub (0n, 1n, name)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let name  : string = "Alice"
let slice : string = String.sub 0n 1n name
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let name  : string = "Alice";
let slice : string = String.sub (0n, 1n, name);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let name: string = "Alice";
let slice: string = String.sub (0 as nat, 1 as nat, name);
```

</Syntax>


> ⚠️ Notice that the offset and length of the slice are natural
> numbers.

### Length of Strings

The length of a string can be found using a built-in function:


<Syntax syntax="pascaligo">

```pascaligo group=c
const name : string = "Alice"
const length : nat = String.length (name) // length = 5
```

> Note that `size` is *deprecated*. 

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let name : string = "Alice"
let length : nat = String.length name  // length = 5
```

> Note that `String.size` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let name : string = "Alice";
let length : nat = String.length (name);  // length == 5
```

> Note that `String.size` is *deprecated*.

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let name: string = "Alice";
let length: nat = String.length(name);  // length == 5
```

</Syntax>

## Bytes

Byte literals are defined using the prefix `0x` followed by hexadecimal digits like this:


<Syntax syntax="pascaligo">

```pascaligo
const b : bytes = 0x7070
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let b : bytes = 0x7070
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let b : bytes = 0x7070;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let b : bytes = 0x7070;
```

</Syntax>



### Concatenating Bytes

Bytes can be concatenated using the `Bytes.concat` function.

<Syntax syntax="pascaligo">

```pascaligo group=d
const white : bytes = 0xffff
const black : bytes = 0x0000
const mixed : bytes = Bytes.concat (white, black) // 0xffff0000
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=d
let white : bytes = 0xffff
let black : bytes = 0x0000
let mixed : bytes = Bytes.concat white black (* 0xffff0000 *)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=d
let white : bytes = 0xffff;
let black : bytes = 0x0000;
let mixed : bytes = Bytes.concat(white, black); // 0xffff0000
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=d
let white: bytes = 0xffff;
let black: bytes = 0x0000;
let mixed: bytes = Bytes.concat(white, black); // 0xffff0000
```

</Syntax>



### Extracting Bytes

Bytes can be extracted using the predefined function `Bytes.sub`. 
The first parameter takes the start index and the second parameter takes the number of bytes.
Pay special attention to how `bytes` are indexed.

<Syntax syntax="pascaligo">

```pascaligo group=e
const b     : bytes = 0x12345678
const slice : bytes = Bytes.sub (1n, 2n, b) // 0x3456
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=e
let b     : bytes = 0x12345678
let slice : bytes = Bytes.sub 1n 2n b (* 0x3456 *)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=e
let b     : bytes = 0x12345678;
let slice : bytes = Bytes.sub (1n, 2n, b); // 0x3456
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=e
let b     : bytes = 0x12345678;
let slice : bytes = Bytes.sub (1 as nat, 2 as nat, b); // 0x3456
```

</Syntax>

### Length of Bytes

The length of `bytes` can be found using a built-in function `Bytes.length`:


<Syntax syntax="pascaligo">

```pascaligo group=f
const b      : bytes = 0x123456
const length : nat   = Bytes.length (b) // length = 3
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=f
let b      : bytes = 0x123456
let length : nat   = Bytes.length b  (* length = 3 *)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=f
let b      : bytes = 0x123456;
let length : nat   = Bytes.length (b);  // length = 3
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=f
let b      : bytes = 0x123456;
let length : nat   = Bytes.length(b);  // length = 3
```

</Syntax>

