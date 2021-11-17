---
id: philosophy
title: Philosophy
---

To understand LIGO’s design choices it’s important to understand its philosophy. We have two main concerns in mind while building LIGO.

## Safety
Once a smart contract is deployed, it will likely be impossible to change it. You must get it right on the first try, and LIGO should help as much as possible. There are multiple ways to make LIGO a safer language for smart contracts.

### Automated Testing
Automated Testing is the process through which a program runs another program, and checks that this other program behaves correctly.  

There already is a testing library for LIGO programs written in OCaml that is used to test LIGO itself. Making it accessible to users will greatly improve safety. A way to do so would be to make it accessible from within LIGO.

### Static Analysis
Static analysis is the process of having a program analyse another one.
For instance, type systems are a kind of static analysis through which it is possible to find lots of bugs. LIGO already has a simple type system, and we plan to make it much stronger.

### Conciseness
Writing less code gives you less room to introduce errors. That's why LIGO encourages writing lean rather than chunky smart contracts.

---

## Ergonomics
Having an ergonomic product is crucial on multiple levels:
Making features easily accessible ensures they’ll actually get used.
Not wasting users time on idiosyncrasies frees more time for making contracts safer or building apps.
Keeping users in a Flow state makes it possible to introduce more complex features in the language.
There are multiple ways to improve ergonomics.

### The Language
LIGO should contain as few surprises as possible. This is usually known as the principle of least surprise.

Most programmers who will use LIGO have already spent a lot of time learning to develop in an existing language, with its own set of conventions and expectations. These expectations are often the most important to accommodate. This is why C-style syntaxes are especially popular (e.g. JavaScript), C-style is well known and new languages want to take advantage of that familiarity. Therefore as an extension of the principle of least surprise, LIGO supports more than one syntax. The least surprising language for a new developer is the one that they have already learned how to use. It’s probably not practical to replicate the syntax of every programming language, so LIGO takes the approach of replicating the structure used by languages from a particular paradigm. 

It is packaged in a Docker container, so that no particular installation instructions are required.

### Editor Support
Without editor support, a lot of manipulations are very cumbersome. Checking for errors, testing, examining code, refactoring code, etc. This is why there is ongoing work on editor support, starting with highlighting and code-folding.

### Docs
Docs include documentation of the languages, tutorials, as well as examples and design patterns.
We’re a long way from there. But having extensive docs is part of our goals.