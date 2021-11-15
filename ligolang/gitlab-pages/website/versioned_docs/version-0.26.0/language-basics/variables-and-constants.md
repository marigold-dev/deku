---
id: constants-and-variables
title: Constants & Variables
---

import Syntax from '@theme/Syntax';


The next building block after types are *constants* and *variables*.

## Constants

Constants are immutable by design, which means their values cannot be
reassigned. Put in another way, they can be assigned once, at their
declaration. When defining a constant you need to provide a `name`,
`type` and a `value`:


<Syntax syntax="pascaligo">

```pascaligo group=a
const age : int = 25
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo --entry-point age
# Outputs: 25
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let age : int = 25
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.mligo --entry-point age
# Outputs: 25
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=a
let age : int = 25;
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.religo --entry-point age
# Outputs: 25
```

</Syntax>
<Syntax syntax="jsligo">

> Constants in JsLIGO are enforced:

```jsligo skip
let x = (a: int): int => {
  const age : int = 25;
  age = 3; // gives an error
};
```

Unlike the other syntaxes, JsLIGO doesn't allow variable names to be reused in the same block scope:

```jsligo skip
let x = (a: int): int => {
  const age: int = 25;
  const age: int = 3; // will give an error
};
```

However, the following does work:

```jsligo group=d
let x = (a: int): int => {
  const age: int = 25;
  {
     const age: int = 3; // does not give an error
     return age;
  }
};
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.jsligo --entry-point age
# Outputs: 25
```

</Syntax>


## Variables


<Syntax syntax="pascaligo">

Variables, unlike constants, are *mutable*. They cannot be declared in
a *global scope*, but they can be declared and used within functions,
or as function parameters.

> ⚠️ Please be wary that mutation only works within the function scope
> itself, values outside of the function scope will not be
> affected. In other words, when a function is called, its arguments
> are copied, *as well as the environment*. Any side-effect to that
> environment is therefore lost when the function returns.


```pascaligo group=b
// The following is invalid: use `const` for global values instead.
// var four : int := 4

function add (const a : int; const b : int) : int is
  block {
    var c : int := a + 2*b;
    c := c - b
  } with c
```

> ⚠️ Notice the assignment operator `:=` for `var`, instead of `=` for
> constants.

You can run the `add` function defined above using the LIGO compiler
like this:

```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.ligo '(1,1)' --entry-point add
# Outputs: 2
```

</Syntax>
<Syntax syntax="cameligo">

As expected in the pure subset of a functional language, CameLIGO only
features *constant values*: once they are declared, the value cannot
be changed (or "mutated").

```cameligo group=c
let add (a : int) (b : int) : int =
  let c : int = a + b in c
```

You can run the `add` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.mligo '(1,1)' --entry-point add
# Outputs: 2
```

</Syntax>
<Syntax syntax="reasonligo">

As expected in the pure subset of a functional language, ReasonLIGO
only features *constant values*: once they are declared, the value
cannot be changed (or "mutated").

```reasonligo group=c
let add = ((a, b): (int, int)): int => {
  let c : int = a + b;
  c;
};
```

You can run the `add` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.religo '(1,1)' --entry-point add
# Outputs: 2
```

</Syntax>
<Syntax syntax="jsligo">

Variables, unlike constants, are *mutable*. 

> ⚠️ Please be wary that mutation only works within the function scope
> itself, values outside of the function scope will not be
> affected. In other words, when a function is called, its arguments
> are copied, *as well as the environment*. Any side-effect to that
> environment is therefore lost when the function returns.


```jsligo group=b
let add = (a: int, b: int): int => {
  let c = a;
  c = c + b;
  return c
}
```

You can run the `add` function defined above using the LIGO compiler
like this:

```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.jsligo '(1,1)' --entry-point add
# Outputs: 2
```


</Syntax>

