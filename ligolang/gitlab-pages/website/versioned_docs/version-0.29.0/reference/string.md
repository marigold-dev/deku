---
id: string-reference
title: String
description: Operations for strings.
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
function length : string -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val length : string -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let length: string => nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let length: (s: string) => nat
</SyntaxTitle>

Get the size of a string. 

[Michelson only supports ASCII strings](http://tezos.gitlab.io/whitedoc/michelson.html#constants) 
so for now you can assume that each character takes one byte of storage.

<Syntax syntax="pascaligo">

```pascaligo
function string_size (const s: string) : nat is String.length(s)
```

> Note that `size` and `String.size` are *deprecated*. 

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let size_op (s: string) : nat = String.length s
```

> Note that `String.size` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let size_op = (s: string): nat => String.length(s);
```

> Note that `String.size` is *deprecated*.

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let size_op = (s: string): nat => String.length(s);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function sub : nat -> nat -> string -> string
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val sub : nat -> nat -> string -> string
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let sub: (nat, nat, string) => string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sub: (offset: nat, length: nat, s: string) => string
</SyntaxTitle>

Extract a substring from a string based on the given offset and length. For 
example the string "abcd" given to the function below would return "bc".


<Syntax syntax="pascaligo">

```pascaligo
function slice_op (const s : string) : string is String.sub(1n , 2n , s)
```

> Note that `string_slice` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let slice_op (s: string) : string = String.sub 1n 2n s
```

> Note that `String.slice` is *deprecated*.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let slice_op = (s: string): string => String.sub(1n, 2n, s);
```

> Note that `String.slice` is *deprecated*.

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let slice_op = (s: string): string => String.sub(1 as nat, 2 as nat, s);
```

</Syntax>



<SyntaxTitle syntax="pascaligo">
function concat : string -> string -> string
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val concat : string -> string -> string
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let concat: (string, string) => string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let concat: (a: string, b: string) => string
</SyntaxTitle>

Concatenate two strings and return the result.



<Syntax syntax="pascaligo">

```pascaligo
function concat_op (const s : string) : string is String.concat(s, "toto")
```

Alternatively:

```pascaligo
function concat_op_alt (const s : string) : string is s ^ "toto"
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let concat_syntax (s: string) = String.concat s "test_literal"
```

Alternatively:

```cameligo
let concat_syntax_alt (s: string) = s ^ "test_literal"
```


</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let concat_syntax = (s: string) => String.concat(s, "test_literal");
```

Alternatively:

```reasonligo
let concat_syntax_alt = (s: string) => s ++ "test_literal";
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let concat_syntax = (s: string): string => String.concat(s, "test_literal");
```

Alternatively:

```jsligo
let concat_syntax_alt = (s: string): string => s + "test_literal";
```

</Syntax>

