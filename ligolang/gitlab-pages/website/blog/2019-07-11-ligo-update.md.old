---
title: Updates about LIGO and Marigold
author: Gabriel Alfour
---

# Updates about LIGO and Marigold

---

It has been a few weeks since our last update. Since then, we've onboarded new collaborators to both LIGO and Marigold, rewritten much of the codebase, and we've begun some exciting new projects. Let's tell you all about it!

# LIGO

Now that we've expanded the team, LIGO is progressing faster! Since our last update, we've published some initial tutorials, streamlined the installation process, and added new features to LIGO.

Our ongoing efforts focus on removing the "warts" of LIGO, i.e. the aspects of LIGO which remain incomplete or unpleasant. Once finished, we will communicate much more extensively about LIGO, how developers can get started, and integrate with popular developer tools.

We are also working on some longer-term projects which we highlight below.

## Generic Front End

LIGO currently supports 2 syntaxes, but that support is clunky and unscaleable to maintain in the long-run. For example, adding a new operator requires us to add it to both syntaxes manually and adding a new syntax remains time-consuming and compounds technical debt.

As such, we are working on a Generic Front End (GFE), so that it becomes much easier to add syntaxes to LIGO and add new features to all syntaxes at once. The GFE also aims to support seamless translation between the syntaxes, so that one can not only write code in any syntax, but also read code written by other people in the syntax of their choice!

To attract Ethereum developers, we are also looking at supporting the syntax of [Yul, an intermediary language between Solidity and the EVM](https://solidity.readthedocs.io/en/v0.5.3/yul.html), which would be a big step in supporting contracts written in Solidity!

## Super Type System

LIGO currently has a very simple type-system, requiring some extraneous type annotations and forbidding a lot of harmless programs.

To fix this, we are putting effort into developing a Super Type System (STS). A more comprehensive type system will also help us to natively support Yul and constructs coming from other popular languages.

Coming at this from the other end, the STS will make it much easier for developers to integrate tools and static analysis into LIGO.

## Formally Verified Backend

The most brittle part of our code base is about to become its strongest part. We are currently rewriting the backend of LIGO in Coq, and partially proving its correctness along the way.

**The significance of this effort can't be stressed enough.** Basically, once we prove the equivalence between a part of LIGO and its Michelson counterpart, we can safely trust it. 

Concretely:
- Running LIGO-in-Browser will become much easier. Instead of having to dry-run it remotely or to rewrite a Michelson interpreter, we'll be able to **directly interpret** the LIGO program.
- It will be possible to prove the properties of smart contracts written in LIGO directly, instead of having to prove the Michelson they produce.
- Fewer tests will ned to be written and testing will instead focus mostly on the developer-facing layers of the compiler (i.e. syntax, typing), rather than on the actual compiling part.

# Marigold

We had slowed development on Marigold until LIGO was ready. While we are still knocking out LIGO's remaining warts, we are finally returning our eyes back to Marigold.

Tangibly speaking, we are locking down some actual implementation details with new collaborators and hope to provide an update in the coming weeks.

On the more theoretical side, we are also working on a mathematical presentation of Plasma. Although there has been a tremendous amount of innovation and tinkering in the Plasma space, current writings about Plasma are very informal and lack mathematical specification. 

It is thus hard for newcomers (even CS researchers!) to dive into Plasma in a common way. It can also be hard to evaluate new ideas in this space, because each Plasma project brings their own jargon, assumptions, and models of how these systems work. Once this is done, we will strive to make Plasma General even more General.

# Contact

If you have any question, feel free to visit [our website](https://ligolang.org) and to contact us :)
