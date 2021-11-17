---
id: strings
title: Strings
---

import Syntax from '@theme/Syntax';

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



## Concatenating Strings


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



## Extracting Substrings

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

## Length of Strings

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

