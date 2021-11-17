---
id: overview
title: Overview
---

After going through the design principles and a few considerations linked to them, getting the Big Picture needs diving in technical matters.

![LIGO Overview](/img/big-picture-overview.png)

As shown in the Schema, LIGO’s compiler is separated in roughly 3 separate parts:

- The Middle End. This is the core of LIGO. It defines its core data-structure (the Common AST), and its type-checking algorithm.

- The Front End. This is the bridge between a given syntax and the Common AST.

- The Back End. This is the bridge between the Common AST and a given compilation target. Currently, our only target is Michelson, but we’ll also target Marigold, and if Tezos moves to Web Assembly, Web Assembly.
