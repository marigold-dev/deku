---
id: bitwise-reference
title: Bitwise
description: Operations on bytes
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
function and : 'a -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val and : 'a -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let and: ('a, nat) => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let and: (a: &apos;a, b: nat) => nat
</SyntaxTitle>

`'a` can either be an `int` or `nat`.

A bitwise `and` operation.

<Syntax syntax="pascaligo">

```pascaligo
const zero: nat = Bitwise.and(2n, 1n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let zero: nat = Bitwise.and 2n 1n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let zero: nat = Bitwise.and(2n, 1n);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let zero: nat = Bitwise.and(2 as nat, 1 as nat);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function or : nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val or :  nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let or: (nat, nat) => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let or: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise `or` operation.

<Syntax syntax="pascaligo">

```pascaligo
const three: nat = Bitwise.or(2n, 1n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let three: nat = Bitwise.or 2n 1n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let three: nat = Bitwise.or(2n, 1n);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let three: nat = Bitwise.or(2 as nat, 1 as nat);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function xor : nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val xor :  nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let xor: (nat, nat) => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let xor: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise `xor` operation.

<Syntax syntax="pascaligo">

```pascaligo
const three: nat = Bitwise.xor(2n, 1n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let three: nat = Bitwise.xor 2n 1n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let three: nat = Bitwise.xor(2n, 1n);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=other
let three: nat = Bitwise.xor(2 as nat, 1 as nat);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function shift_left : nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val shift_left :  nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let shift_left: (nat, nat) => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let shift_left: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise shift left operation.

<Syntax syntax="pascaligo">

```pascaligo
const four: nat = Bitwise.shift_left(2n, 1n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let four: nat = Bitwise.shift_left 2n 1n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let four: nat = Bitwise.shift_left(2n, 1n);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let four: nat = Bitwise.shift_left(2 as nat, 1 as nat);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function shift_right : nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val shift_right :  nat -> nat -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let shift_right: (nat, nat) => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let shift_right: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise shift right operation.

<Syntax syntax="pascaligo">

```pascaligo
const one: nat = Bitwise.shift_right(2n, 1n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let one: nat = Bitwise.shift_right 2n 1n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let one: nat = Bitwise.shift_right(2n, 1n);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let one: nat = Bitwise.shift_right(2 as nat, 1 as nat);
```

</Syntax>