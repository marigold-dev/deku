---
id: include
title: Including Other Contracts
---

import Syntax from '@theme/Syntax';

Let us say that we have a contract that is getting a too large. If it
has a modular structure, you might find it useful to use the
`#include` statement to split the contract up over multiple files.

You take the code that you want to include and put it in a separate
file, for example `included.ligo`:



<Syntax syntax="pascaligo">

```pascaligo

// Demonstrate PascaLIGO inclusion statements, see includer.ligo

const foo : int = 144
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
// Demonstrate CameLIGO inclusion statements, see includer.mligo

let foo : int = 144
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
// Demonstrate ReasonLIGO inclusion statements, see includer.religo

let foo : int = 144;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
// Demonstrate JsLIGO inclusion statements, see includer.jsligo

let foo: int = 144;
```

</Syntax>



And then you can include this code using the `#include` statement like so:



<Syntax syntax="pascaligo">

```pascaligo skip
#include "included.ligo"

const bar : int = foo
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
#include "included.mligo"

let bar : int = foo
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
#include "included.religo"

let bar : int = foo;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
#include "included.jsligo"

let bar: int = foo;
```

</Syntax>
