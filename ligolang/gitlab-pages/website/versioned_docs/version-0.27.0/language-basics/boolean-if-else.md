---
id: boolean-if-else
title: Booleans and Conditionals
---

import Syntax from '@theme/Syntax';

## Booleans

The type of a boolean value is `bool`. Here is how to define a boolean
value:


<Syntax syntax="pascaligo">

```pascaligo group=a
const a : bool = True   // Also: true
const b : bool = False  // Also: false
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let a : bool = true
let b : bool = false
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=a
let a : bool = true;
let b : bool = false;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=a
let a: bool = true;
let b: bool = false;
```

</Syntax>

Common operations:

<Syntax syntax="pascaligo">
<div className="boolean-example-table">
  <div className="operation">
    and
  </div>
  <div className="description">
    Logical and
  </div>
  <div className="example">

```pascaligo
const logical_and: bool = True and True;
```

  </div>
  <div className="operation">
    or
  </div>
  <div className="description">
    Logical or
  </div>
  <div className="example">

```pascaligo
const logical_or: bool = False or True;
```

  </div>
  <div className="operation">
    not
  </div>
  <div className="description">
    Logical not
  </div>
  <div className="example">

```pascaligo
const logical_not: bool = not False;
```

  </div>
  <div className="operation">
    =
  </div>
  <div className="description">
    Equals
  </div>
  <div className="example">

```pascaligo
const eq: bool = 2 = 3;
```

  </div>  
  <div className="operation">
    =/=
  </div>
  <div className="description">
    Not equals
  </div>
  <div className="example">

```pascaligo
const not_eq: bool = 2 =/= 3;
```

  </div>
  <div className="operation">
    &gt;
  </div>
  <div className="description">
    Greater than
  </div>
  <div className="example">

```pascaligo
const gt: bool = 4 > 3;
```

  </div>  
  <div className="operation">
    &lt;
  </div>
  <div className="description">
    Less than
  </div>
  <div className="example">

```pascaligo
const lt: bool = 4 < 3;
```

  </div>  
  <div className="operation">
    &gt;=
  </div>
  <div className="description">
    Greater than or equal to
  </div>
  <div className="example">

```pascaligo
const gte: bool = 4 >= 3;
```

  </div>    
  <div className="operation">
    &lt;=
  </div>
  <div className="description">
    Less than or equal to
  </div>
  <div className="example">

```pascaligo
const lte: bool = 4 <= 3;
```

  </div>    
</div>
</Syntax>

<Syntax syntax="cameligo">
<div className="boolean-example-table">
  <div className="operation">
    &&
  </div>
  <div className="description">
    Logical and
  </div>
  <div className="example">

```cameligo
let logical_and: bool = true && true
```

  </div>
  <div className="operation">
    ||
  </div>
  <div className="description">
    Logical or
  </div>
  <div className="example">

```cameligo
let logical_or: bool = false || true
```

  </div>
  <div className="operation">
    !
  </div>
  <div className="description">
    Logical not
  </div>
  <div className="example">

```cameligo
let logical_not: bool = not false
```

  </div>
  <div className="operation">
    =
  </div>
  <div className="description">
    Equals
  </div>
  <div className="example">

```cameligo
let eq: bool = 2 = 3
```

  </div>  
  <div className="operation">
    &lt;&gt;
  </div>
  <div className="description">
    Not equals
  </div>
  <div className="example">

```cameligo
let not_eq: bool = 2 <> 3
```

  </div>
  <div className="operation">
    &gt;
  </div>
  <div className="description">
    Greater than
  </div>
  <div className="example">

```cameligo
let gt: bool = 4 > 3
```

  </div>  
  <div className="operation">
    &lt;
  </div>
  <div className="description">
    Less than
  </div>
  <div className="example">

```cameligo
let lt: bool = 4 < 3
```

  </div>  
  <div className="operation">
    &gt;=
  </div>
  <div className="description">
    Greater than or equal to
  </div>
  <div className="example">

```cameligo
let gte: bool = 4 >= 3
```

  </div>    
  <div className="operation">
    &lt;=
  </div>
  <div className="description">
    Less than or equal to
  </div>
  <div className="example">

```cameligo
let lte: bool = 4 <= 3
```

  </div>    
</div>
</Syntax>

<Syntax syntax="reasonligo">
<div className="boolean-example-table">
  <div className="operation">
    &&
  </div>
  <div className="description">
    Logical and
  </div>
  <div className="example">

```reasonligo
let logical_and: bool = true && true;
```

  </div>
  <div className="operation">
    ||
  </div>
  <div className="description">
    Logical or
  </div>
  <div className="example">

```reasonligo
let logical_or: bool = false || true;
```

  </div>
  <div className="operation">
    !
  </div>
  <div className="description">
    Logical not
  </div>
  <div className="example">

```reasonligo
let logical_not: bool = !false;
```

  </div>
  <div className="operation">
    ==
  </div>
  <div className="description">
    Equals
  </div>
  <div className="example">

```reasonligo
let eq: bool = 2 == 3;
```

  </div>  
  <div className="operation">
    !=
  </div>
  <div className="description">
    Not equals
  </div>
  <div className="example">

```reasonligo
let not_eq: bool = 2 != 3;
```

  </div>
  <div className="operation">
    &gt;
  </div>
  <div className="description">
    Greater than
  </div>
  <div className="example">

```reasonligo
let gt: bool = 4 > 3;
```

  </div>  
  <div className="operation">
    &lt;
  </div>
  <div className="description">
    Less than
  </div>
  <div className="example">

```reasonligo
let lt: bool = 4 < 3;
```

  </div>  
  <div className="operation">
    &gt;=
  </div>
  <div className="description">
    Greater than or equal to
  </div>
  <div className="example">

```reasonligo
let gte: bool = 4 >= 3;
```

  </div>    
  <div className="operation">
    &lt;=
  </div>
  <div className="description">
    Less than or equal to
  </div>
  <div className="example">

```reasonligo
let lte: bool = 4 <= 3;
```

  </div>    
</div>
</Syntax>


<Syntax syntax="jsligo">
<div className="boolean-example-table">
  <div className="operation">
    &&
  </div>
  <div className="description">
    Logical and
  </div>
  <div className="example">

```jsligo
let logical_and: bool = true && true;
```

  </div>
  <div className="operation">
    ||
  </div>
  <div className="description">
    Logical or
  </div>
  <div className="example">

```jsligo
let logical_or: bool = false || true;
```

  </div>
  <div className="operation">
    !
  </div>
  <div className="description">
    Logical not
  </div>
  <div className="example">

```jsligo
let logical_not: bool = !false;
```

  </div>
  <div className="operation">
    ==
  </div>
  <div className="description">
    Equals
  </div>
  <div className="example">

```jsligo
let eq: bool = 2 == 3;
```

  </div>  
  <div className="operation">
    !=
  </div>
  <div className="description">
    Not equals
  </div>
  <div className="example">

```jsligo
let not_eq: bool = 2 != 3;
```

  </div>
  <div className="operation">
    &gt;
  </div>
  <div className="description">
    Greater than
  </div>
  <div className="example">

```jsligo
let gt: bool = 4 > 3;
```

  </div>  
  <div className="operation">
    &lt;
  </div>
  <div className="description">
    Less than
  </div>
  <div className="example">

```jsligo
let lt: bool = 4 < 3;
```

  </div>  
  <div className="operation">
    &gt;=
  </div>
  <div className="description">
    Greater than or equal to
  </div>
  <div className="example">

```jsligo
let gte: bool = 4 >= 3;
```

  </div>    
  <div className="operation">
    &lt;=
  </div>
  <div className="description">
    Less than or equal to
  </div>
  <div className="example">

```jsligo
let lte: bool = 4 <= 3;
```

  </div>    
</div>
</Syntax>


## Comparing Values

In LIGO, only values of the same type can be compared. Moreover, not
all values of the same type can be compared, only those with
*comparable types*, which is a concept lifted from
Michelson. Comparable types include, for instance, `int`, `nat`,
`string`, `tez`, `timestamp`, `address`, etc. As an example of
non-comparable types: maps, sets or lists are not comparable: if you
wish to compare them, you will have to write your own comparison
function.

### Comparing Strings


<Syntax syntax="pascaligo">

```pascaligo group=b
const a : string = "Alice"
const b : string = "Alice"
const c : bool = (a = b) // True
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
let a : string = "Alice"
let b : string = "Alice"
let c : bool = (a = b) // true
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=b
let a : string = "Alice";
let b : string = "Alice";
let c : bool = (a == b); // true
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=b
let a: string = "Alice";
let b: string = "Alice";
let c: bool = (a == b); // true
```

</Syntax>


### Comparing numbers


<Syntax syntax="pascaligo">

```pascaligo group=c
const a : int  = 5
const b : int  = 4
const c : bool = (a = b)
const d : bool = (a > b)
const e : bool = (a < b)
const f : bool = (a <= b)
const g : bool = (a >= b)
const h : bool = (a =/= b)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let a : int  = 5
let b : int  = 4
let c : bool = (a = b)
let d : bool = (a > b)
let e : bool = (a < b)
let f : bool = (a <= b)
let g : bool = (a >= b)
let h : bool = (a <> b)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=c
let a : int  = 5;
let b : int  = 4;
let c : bool = (a == b);
let d : bool = (a > b);
let e : bool = (a < b);
let f : bool = (a <= b);
let g : bool = (a >= b);
let h : bool = (a != b);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
let a: int  = 5;
let b: int  = 4;
let c: bool = (a == b);
let d: bool = (a > b);
let e: bool = (a < b);
let f: bool = (a <= b);
let g: bool = (a >= b);
let h: bool = (a != b);
```

</Syntax>

### Comparing tez

> 💡 Comparing `tez` values is especially useful when dealing with an
> amount sent in a transaction.


<Syntax syntax="pascaligo">

```pascaligo group=d
const a : tez  = 5mutez
const b : tez  = 10mutez
const c : bool = (a = b) // False
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=d
let a : tez  = 5mutez
let b : tez  = 10mutez
let c : bool = (a = b) // false
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=d
let a : tez  = 5mutez;
let b : tez  = 10mutez;
let c : bool = (a == b); // false
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=d
let a: tez  = 5 as mutez;
let b: tez  = 10 as mutez;
let c: bool = (a == b); // false
```

</Syntax>



## Conditionals

Conditional logic enables forking the control flow depending on the
state.


<Syntax syntax="pascaligo">

```pascaligo group=e
type magnitude is Small | Large // See variant types.

function compare (const n : nat) : magnitude is
  if n < 10n then Small else Large
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/boolean-if-else/cond.ligo '21n' --entry-point compare
# Outputs: Large(Unit)
```

When the branches of the conditional are not a single expression, as
above, we need a block:

```pascaligo skip
if x < y then
  block {
    const z : nat = x;
    x := y; y := z
  }
else skip;
```

As an exception to the rule, the blocks in a conditional branch do not
need to be introduced by the keyword `block`, so we could have written
instead:
```pascaligo skip
if x < y then {
  const z : nat = x;
  x := y; y := z
}
else skip;
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=e
type magnitude = Small | Large // See variant types.

let compare (n : nat) : magnitude =
  if n < 10n then Small else Large
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/boolean-if-else/cond.mligo '21n' --entry-point compare
# Outputs: Large
```

> Notice that, as in OCaml, in CameLIGO, if a conditional has a branch
> `else ()`, that branch can be omitted. The resulting so-called
> *dangling else* problem is parsed by associating any `else` to the
> closest previous `then`.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=e
type magnitude = Small | Large; // See variant types.

let compare = (n : nat) : magnitude =>
  if (n < 10n) { Small; } else { Large; };
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/boolean-if-else/cond.religo '21n' --entry-point compare
# Outputs: Large
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=e
type magnitude = ["Small"] | ["Large"]; // See variant types.

let compare = (n : nat) : magnitude => {
  if (n < (10 as nat)) { return Small (); } else { return Large (); };
};
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/boolean-if-else/cond.religo '21n' --entry-point compare
# Outputs: Large
```

</Syntax>


