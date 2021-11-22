---
id: ligo-snippets-demo
title: Ligo-Snippets Demo
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

“ligo-snippets” (https://www.npmjs.com/package/@ligolang/ligo-snippets) is a React component that can be included on any webpage to display Ligo source code to users. 

The user will see Ligo code with syntax highlighting, and an action button allowing the user to open the source code in the Ligo Web IDE (https://ide.ligolang.org).

Each code snippet can have preset Ligo Web IDE configurations (e.g. entrypoint, parameters or storage). These configurations are optional and will be passed onto the Ligo Web IDE when present. This will allow examples to provide the proper configurations for the reader to experiment with. 

The “ligo-snippets” React component uses the CodeJar editor (https://github.com/antonmedv/codejar), which is extremely lightweight (only 2kB).  It currently supports syntax highlighting for PascaLigo, CameLigo and ReasonLigo. Additionally, it has both a light and dark theme mode. 



<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo {"name": "Ligo Introduction Example", "editor": true}
(*_*
  name: PascaLIGO Contract
  language: pascaligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment (1)
    storage: 999
  deploy:
    entrypoint: main
    storage: 999
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: (5, 6)
  generateDeployScript:
    entrypoint: main
    storage: 999
*_*)
// variant defining pseudo multi-entrypoint actions
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
  block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
  block { skip } with a - b

// real entrypoint that re-routes the flow based
// on the action provided
function main (const p : action ; const s : int) :
  (list(operation) * int) is
  block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> add(s, n)
  | Decrement(n) -> subtract(s, n)
  end)

```

</TabItem>
<TabItem value="cameligo">

```cameligo {"name": "Ligo Introduction Example", "editor": true}
(*_*
  name: CameLIGO Contract
  language: cameligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment 1
    storage: 999
  deploy:
    entrypoint: main
    storage: 999
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: 5, 6
  generateDeployScript:
    entrypoint: main
    storage: 999
*_*)
type storage = int

(* variant defining pseudo multi-entrypoint actions *)

type action =
| Increment of int
| Decrement of int

let add (a,b: int * int) : int = a + b
let sub (a,b: int * int) : int = a - b

(* real entrypoint that re-routes the flow based on the action provided *)

let main (p,s: action * storage) =
 let storage =
   match p with
   | Increment n -> add (s, n)
   | Decrement n -> sub (s, n)
 in ([] : operation list), storage

```

</TabItem>
<TabItem value="reasonligo">

```reasonligo {"name": "Ligo Introduction Example", "editor": true}
(*_*
  name: ReasonLIGO Contract
  language: reasonligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment (1)
    storage: 999
  deploy:
    entrypoint: main
    storage: 999
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: (5, 6)
  generateDeployScript:
    entrypoint: main
    storage: 999
*_*)
type storage = int;

/* variant defining pseudo multi-entrypoint actions */

type action =
  | Increment(int)
  | Decrement(int);

let add = ((a,b): (int, int)): int => a + b;
let sub = ((a,b): (int, int)): int => a - b;

/* real entrypoint that re-routes the flow based on the action provided */

let main = ((p,storage): (action, storage)) => {
  let storage =
    switch (p) {
    | Increment(n) => add((storage, n))
    | Decrement(n) => sub((storage, n))
    };
  ([]: list(operation), storage);
};

```
</TabItem>
</Tabs>
