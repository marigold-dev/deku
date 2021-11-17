---
id: getting-started
title: Getting started
---

## Where
LIGO is big, so it is easier to focus on a specific part of LIGO. As starting suggestions:
* If you want to immediately see the result of your contributions, you might want to start with the Front-End. This is what most people will directly see of LIGO.
* If you want to get into Programming Language Theory, you might want to start with the Middle-End. This is where the Type System and the Language Definition are (the closest thing so far that you’ll find in a research paper).
* If you want to get into the nitty gritty details of compiling to special targets, focus on the Back-End.
* If you want to develop tooling on top of LIGO (editor integration for instance), look at `Ast_typed/types.ml`. This is where most information that is relevant to devs will be (for now).
* If you want to get a grasp of the whole pipeline, search for issues tagged with “Everything.” They’ll help you look at multiple parts of the code base.

## What
The first issues will most likely be:
* Adding tests
* Extending the languages by adding new operators
* *Adding tests*
* Refactoring
* Writing documentation and tutorials for users
* **_Adding tests_**
* Writing internal documentation when you understand a part of the code base
>Tests are **really** important, we don’t have lots of them, and mostly regression ones. This can’t be stressed enough. Some features are missing not because we can’t add them, but because we don’t know as no tests tell us they are missing.

## How
Issues will be added to GitLab tagged with `On-boarding and Front-End` / `Middle-End` / `Back-End` / `Everything`.

If you try to tackle an issue and you have **any** problem, please tell us by creating a new GitLab issue, contacting us on Riot, on Discord, or even by mail!

Problems might include:
* Installing the repository or the tools needed to work on it
* OCaml weirdness
* Understanding undocumented parts of the code base
* Understanding documented parts of the code base

**Anything, really.**

---

## FAQ

### I don’t know much about OCaml. Where should I start? What should I keep in mind?
I’d suggesting going through Real World OCaml to get a feel for the language, to know its features, and to know what to Google when you’re lost.
Beyond that, I’d say, start hacking! Either on LIGO’s code base or on any personal project.
There is a Discord server if you want real-time help (which makes things go way faster than waiting for an answer on StackOverflow or staring mindlessly at the monitor).

### I want to add [X] to LIGO! Where should I begin?
Trying to add a new feature from scratch instead of building on one can be complicated. However, if you’re motivated, contact us! We’ll tell you what we see as the best plan to get the result you want.

