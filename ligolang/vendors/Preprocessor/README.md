# The Preprocessor Library and Standalone Executable

A preprocessor is a tool that reads a text file and, if some special
instructions, called _preprocessing directives_, are found in the
input, the output may not be an identical copy, for example some parts
can be skipped. We document here the preprocessor shipped with the
LIGO compiler.

## Contents

This directory is located in the `vendors` directory of the repository
of LIGO, here `ligo/vendors`. As such, it is distributed with the LIGO
compiler which uses it as a library. Nevertheless, a standalone
preprocessor can also be built and used either as an interactive way
to test the library, or for uses independent of LIGO. The directory
`ligo/vendors/Preprocessor` should list the following files:

```
Preprocessor
├── API.mli
├── API.mll
├── CLI.ml
├── CLI.mli
├── dune
├── dune-project
├── E_AST.ml
├── E_LexerMain.ml
├── E_Lexer.mli
├── E_Lexer.mll
├── E_ParserMain.ml
├── E_Parser.mly
├── Error.ml
├── Error.mli
├── LICENSE
├── Makefile.cfg
├── Preprocessor.opam
├── PreprocMainGen.ml
├── PreprocMainGen.mli
├── PreprocMain.ml
├── README.md
├── State.ml
├── State.mli
└── Tests
     ├── ...
     ...
```

Here is a short description of those files:

  * The OCaml module `API` is the heart of the preprocessor.

  * The modules whose names start with `E_` are used by `API` to parse
    the boolean expression of conditional directives `#if` and
    `#elif`.

  * The module `Error` defines the errors that the preprocessor may
    emit.

  * The `LICENSE` file must contain the MIT license.

  * The modules `CLI` and `PreprocMainGen` deal with command-line
    options for the standalone preprocessor, and also export data
    structures about the configuration meant for the library client,
    for example, the LIGO compiler.

  * The module `PreprocMain` is the standalone preprocessor.

  * The directory `Tests` is used for unit tests of the standalone
    preprocessor.

  * The file `README.md` is the present file.

  * The module `State` defines a type and related functions used for
    preprocessing.

The following files are meant to be used only by a special Makefile to
build the standalone preprocessor. See
[how to build with the Makefile](#the-standalone-preprocessor-with-make).

  * Hidden files prefixed by `.`.

  * The file `Makefile.cfg` configures the Makefile.

## Builds

### The Preprocessor as a Library

The relevant files are `dune` and `dune-project`. The shell command

        $ dune build Preprocessor.a

is to be run in the directory `ligo/vendors/Preprocessor`.

### The Standalone Preprocessor with dune

The relevant files are `dune` and `dune-project`. The shell command

        $ dune build PreprocMain.exe

is to be run in the directory `ligo/vendors/Preprocessor`. As usual
with `dune`, the executable is found in the mirror directory tree
`ligo/_build/default/vendors/Preprocessor`.

### The Standalone Preprocessor with Make

It is possible to build the standalone preprocessor with a Makefile
instead of `dune`, in which case you need to clone the following `git`
repositories first, outside the LIGO working directory:

> $ git clone https://github.com/rinderknecht/Scripts.git

> $ git clone https://github.com/rinderknecht/OCaml-build.git

Make sure the directory to the scripts is in your `PATH` shell
variable. Then, back in `ligo/vendors/Preprocessor`, run

> $ setup.sh

which should create symbolic links or report some errors. Running

> $ make conf

should at least find `ocamlc`, `ocamldep`, `menhir`, `grep`, `sed` and
`arch`. If not, install them.

Then run

> $ make sync

> $ make

which is expected to build `PreprocMain.byte` in the directory
`./_x86_64/`, whose name is made by prefixing with `_` the result of
the output of the shell command

> $ uname --machine

Note: the Makefile relies on the `opam` database installed by
previously running

> $ make

at the root of the LIGO working directory `ligo`, or, at least,

> $ install_vendors_deps.sh

in `ligo/scripts`.

## The Standalone Preprocessor

Let us assume here that the standalone preprocessor has been [built
with `dune`](#the-standalone-preprocessor-with-dune), therefore the
executable is `PreprocMain.exe`. We can obtain the available
command-line options like so:

    $ PreprocMain.exe --help

resulting in the output

```
Usage: PreprocMain.exe [<option> ...] -- [<input>]
where <input> is the source file (default: stdin),
and each <option> (if any) is one of the following:
  -I <paths>       Inclusion paths (colon-separated)
  -h, --help       This help
  -v, --version    Commit hash on stdout
      --cli        Print given options (debug)
      --columns    Columns for source locations
      --show-pp    Print result of preprocessing
```

First, we see that the preprocessor follows the
[GNU getopt](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
conventions on short and long option names. The input is an anonymous
argument that must be preceded by `--`. The options are

  * `-I <paths>` for finding files given to the directive `#include`
    which are not found by appending their path to the current
    directory.

  * `--columns` for using column numbers instead of horizontal
    offsets when reporting errors in the input (the first character
    on a line is on the column 1, but has offset 0).

  * `--show-pp` for actually printing on `stdin` the result of
    preprocessing the input. If not given this option, the
    preprocessor will only print on `stderr` errors, if any.

## The Preprocessing Directives

This preprocessor features different kinds of directives:

  * directives found in the standard preprocessor for the language
    `C#`;

  * a directive from `cpp`, the `C` preprocessor, enabling the textual
    inclusion of files;

  * a directive specific to LIGO to support a minimal module system.

Importantly, [strings](#preprocessing-strings-and-comments) are
handled the way `cpp` does, not `C#`.

In the following subsections, we shall briefly present those
directives. Here, we state some properties which hold for all of
them.

  * They must start with a `#` symbol at the beginning of a line.

  * Wrongly spelled directives or unsupported ones are ignored without
    warning, and therefore will appear in the output.

  * They can have arguments in the form of free text or
    strings. (Anything after the directive name is considered a
    potential argument.)

  * Strings arguments must be enclosed between double quotes and
    cannot span over two or more lines.

  * The valid preprocessing of a directive leaves in its place an
    empty line (that is, a newline character) or another directive, to
    be picked up by other tools, like lexers.

  * Newline characters are never discarded, to preserve the line
    numbers of copied text.

### The Error Directive

When debugging or putting in place directives in an already existing
input, it is sometimes useful to force the preprocessor to stop and
emit an error. This is possible thanks to the `#error` directive,
which is followed by an error message as free text until the end of
the line, like so:

```
#error Not implemented/tested yet
```

### Conditional Directives and Symbol Definition

The preprocessor enables the conditional copying of its input. At the
start, its default mode is _copy_, meaning that characters in the
input are copied to the output. Conditional directives enable another
mode: _skip_, by means of which the following characters are
discarded, and only newline characters are copied.

Conditional directives follow the familiar syntax of some of their
cousins in programming languages. At the very least,

  1. they start with the `#if` directive, followed by a Boolean
     expression as argument,

  2. and they are closed by `#endif`.

It is also possible to use

  * one `#else` directive before `#endif`;

  * a series of `#elif` directive after `#if` (as a short-hand for a
    `#else` immediately followed by an `#if`, except that only one
    `#endif` will close the conditional).

A trivial example would be:


```
#if false
This is NOT copied to the output, except the newline character
#endif
```

where `false` is a predefined symbol acting like a Boolean value. The
output is

```
# 1 "Tests/test.txt"




```

Note what looks like an anonymous preprocessing directive `# 1
"Tests/test.txt"`. We will explain its meaning when presenting
[The Inclusion Directive](#the-inclusion-directive). (Remark: `cpp`
would not output blank lines followed by the end of the file.) Their
use is clearer if we add text before and after the conditional, like
so:

```
---
#if false
A
#endif
---
```

whose preprocessed output is then

```
# 1 "Tests/test.txt"
---



---
```

Here is how to use the `#else` directive:

```
#if false
This is NOT copied to the output, except the newline character.
#else
This IS copied to the output.
#endif
```

The booleans `false` and `true` are predefined. The way to define
_symbols_ (that is the traditional name of those identifiers) consists
in using the `#define` directive, followed by the symbol, like so:

```
#define SYM

#if SYM
This IS copied to the output.
#else
This is NOT copied to the output, except the newline character.
#endif
```

This opens the possibility to use Boolean expressions made of
  * `true` and `false` already mentioned;
  * `||` for the disjunction ("or");
  * `&&` for the conjunction ("and");
  * `==` for equality;
  * `!=` for inequality;
  * `!` for negation;
  * `(` and `)` around expressions to specify priorities.

Directives are processed in sequence in the input file. This
preprocessor, like that of `C#`, allows us to _undefine_ a symbol,
that is, giving it the Boolean value `false`, like so:

```
#define SYM
#undef SYM

#if SYM
This is NOT copied to the output, except the newline character.
#else
This IS copied to the output.
#endif
```

The result is

```
# 1 "Tests/undef.txt"






This IS copied to the output.

```

Note: If you wish to redefine a symbol, you must undefine it first.

When we want to write a cascade of conditionals, the preprocessor
enables a shortcut by means of the `#elif` directive, like so:

```
#if ...
...
#elif ...
...
#elif ...
...
#endif
```

Basically, a `#elif` directive is equivalent to `#else` followed by
`#if`, but we only need to close with only one `#endif`.

The rationale for using conditional directives in LIGO is to enable in
a single smart contract several versions of a standard.

```
#if STANDARD_1
...
#elif STANDARD_2
...
#else
#error Standard not implemented
#endif
```

A real life example could be
[Dexter](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L52). It
provides another interesting use of a conditional directive, where
[a record type depends on the version of the standard](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L84).

### The Inclusion Directive

The solution provided by the conditional directives with symbol
definition to manage several standards is improved upon by physically
separating the input into different files. This is where the
`#include` directive comes handy. Basically, it takes an argument made
of a string containing a path to the file to be textually included,
like so:

```
#include "path/to/standard_1.ligo"
```

and the preprocessor replaces the directive with the contents of the
file `path/to/standard_1.ligo`, whose contents is then preprocessed as
well. This can in theory create a loop, for example, if two files try
to include each other.

In fact, the preprocessor does more than simply include the given
file. To enable the consumer of the output to keep track of
inclusions, in particular, to maintain the line numbers of the input
that has been copied, the preprocessor inserts two special directives
in the output, called
[linemarkers](https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html),
one in the stead of the `#include` directive and one after the
inclusion. Let us consider the following example where `a.txt`
includes `b.txt`, which, in turn, includes `c.txt`. Here is the
contents of `a.txt`:

```
Start of "a.txt"
#include "b.txt"
End of "a.txt"
```

Then `b.txt`:

```
Start of "b.txt"
#include "c.txt"
End of "b.txt"
```

and, finally, `c.txt`:

```
Start of "c.txt"
End of "c.txt"
```

If we gather the files in a `Tests` directory and run the
[standalone preprocessor built with `dune`](#the-standalone-preprocessor-with-dune)
like so

> $ PreprocMain.exe --show-pp -- Tests/a.txt

we obtain on `stdout`:

```
# 1 "Tests/a.txt"
Start of "a.txt"

# 1 "Tests/b.txt" 1
Start of "b.txt"

# 1 "Tests/c.txt" 1
Start of "c.txt"
End of "c.txt"
# 3 "Tests/b.txt" 2
End of "b.txt"
# 3 "Tests/a.txt" 2
End of "a.txt"
```

There are three forms of linemarkers:

  1. `# <line number> "path/to/file"`
  2. `# <line number> "path/to/file" 1`
  2. `# <line number> "path/to/file" 2`

The first kind is used only at the start of the output file and states
that the line after the linemarker has number `<line number>` and
belongs to the file `path/to/file`. Therefore `Start of "a.txt"` has
line number `1` in file `Tests/a.txt`.

The second kind is used when including a file. The `#include`
directive is discarded in the output, except the newline character,
which explains the empty line after `Start of "a.txt"`. Then a
linemarker ending in `1` is printed, which means that we went to the
file `path/to/file` when processing the input.

The third kind is inserted in the output upon returning from an
included file. For example, `# 3 "Tests/b.txt" 2` means that the next
line has number `3` and we return to file `Tests/b.txt`.

Linemarkers need to be handled by the consumer of the output. In the
context of the LIGO compiler, the lexer reads the output of the
preprocessor, therefore scans for linemarkers.

When using the preprocessor with the LIGO compiler, the `#include`
directive can only occur at the top level according to the grammar,
that is, either at the beginning of the smart contract, in between
file-level declarations or at the end. (This property is checked by
the parser.) The rationale for this restriction is to avoid fragments
of smart contracts that are syntactically incorrect, and yet assembled
into a correct one.

### The Import Directive

The `#import` directive is specific to the LIGO compiler. It provides
the support for a
[minimal module system](https://ligolang.org/docs/language-basics/modules#modules-and-imports-build-system).


## Preprocessing Strings and Comments

Strings and comments are recognised by the preprocessor, even in
pieces of the input that are not copied. (This last point is a
difference between `cpp` and the `C#` preprocessor.) The rationale for
doing so when copying the input is twofold:

  1. We do not want the preprocessor to interpret a directive that is
     actually in a comment. This can happen when commenting out a
     piece of the source code that contains a preprocessing directive:
     we do not want that directive to be interpreted.

  2. We do not want the preprocessor to interpret a directive that is
     actually in a string. This can happen if the source code is that
     of a bootstrapped compiler, that is, a compiler for its own
     language. Another scenario is that of a test: the source code is
     actually printing what is happening.

When the processor is in skip mode, that is, the input is not copied,
strings and comments are also recognised. This ensures that a string
or a comment containing a conditional directive, for example `#endif`,
does not start to interact with previous directives, like `#if`, or
raises an error when switching from copy mode to skip mode. In other
words, the interpretation of strings and comments should always be the
same. For example, we want the input

```
#if true
"#endif"
#endif
```

and

```
#if true
"#endif"
#endif
```

to not raise an error.

Strings are enclosed between double quotes.

Comments can follow one of the following combinations:

  * `(*` and `*)` for blocks, and `//` for lines;

  * `/*` and `*/` for blocks, and `//` for lines;

  * `/*` and `*/` for blocks, and `#` for lines

See section [Adding a Comment or String](#adding-a-comment-or-string)
for new combinations.

## Documenting the Modules

### CLI

As we explained in section [Contents](#contents), the module `CLI` has
a twofold purpose: to deal with command-line options for the
standalone preprocessor, and also export data structures about the
configuration meant for the library client, for example, the LIGO
compiler.

#### The Interface

The module signature `COMMENTS` is

```
module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end
```

It exports optional values denoting two kinds of comments: one-line
comments and block comments (that is, multi-line comments). Due to the
use of metaprogramming by means of `ocamllex` to implement the module
`API`, where comments are defined as regular expressions, the client
of `CLI` must make sure to choose line and block comments that are
actually recognised by `API`.

The module signature `S` in `CLI` gathers data structures
parameterising the behaviour of the preprocessor. If building the
standalone preprocessor, those data will come from the
command-line. They are as follows:

```
    val input     : string option (* input file             *)
    val extension : string option (* file extension         *)
    val dirs      : string list   (* -I                     *)
    val show_pp   : bool          (* --show-pp              *)
    val offsets   : bool          (* negation of --columns  *)
```

So, for example, if `offsets` is `true`, error messages will report
regions of the input with horizontal offsets (a la Emacs) instead of
column numbers (a la Vim).

Information about how the parsing of command-line options went is
given by the type and value `status`:

```
    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    val status : status
```

  * The constructor `` `Done`` means that no error occurred.

  * The constructor `` `Version`` carries the commit hash of the source
    code, as shown about
    [the standalone preprocessor](#the-standalone-preprocessor).

  * The `` `Help`` constructor carries the help displayed by `--help`.

  * The constructor `` `CLI`` carries a string buffer containing the
    internal values of the command-line options which were given. This
    is used for debugging the module `CLI`. See `--cli`.

  * The constructor `` `SyntaxError`` reports an error when parsing the
    command-line options.

  * The constructor `` `FileNotFound`` denotes the input file not being
    found.

The rationale for this structure is to enable the CLI to be augmented
when composing the preprocessor with another tool that consumes its
output. For example, the lexing library in `vendors/LexerLib` features
its own `CLI` module, exporting its own type
`status`:

```
    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]
```

which reuses the one in `vendors/Preprocessor/CLI.mli`.

#### The Implementation

On word on the implementation `CLI.ml`. We designed the CLI of the
preprocessor library so it can be composed with other tools, like a
lexer based on the lexer library in `vendors/LexerLib`. In order to
achieve this goal, we do not want the exception `Getopt.Error` to be
raised when finding an unknown option, one that is destined to another
tool, like the lexer. Therefore, we designed a module `Argv` in
`vendors/ligo-utils/simple-utils` that filters out unknown options but
leaves correct ones, even if their syntax is invalid and will result
in an exception `Getopt.Error` raised by
`Getopt.parse_cmdline`. Importantly, we assume that there are no
concatenated short options (here, the only possible combinations are
`-hv` and `-vh`) and that anonymous arguments (here, a unique text
file) is given after `--`.

```
    let opt_wo_arg =
      let open SSet in
      empty
      |> add "--show-pp"
      |> add "--columns"

      (* The following options are present in all CLI *)
      |> add "--cli"                  (* For debugging *)
      |> add "--help" |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg =
      let open SSet in
      empty
      |> add "-I"

    let argv_copy = Array.copy Sys.argv

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg
```

First, we make a backup copy of `Sys.argv`. Second, we filter it in a
list by calling `Argv.filter`. That function performs a side effect on
`Sys.argv`: unknown options are removed and compacted. That is why
`Sys.argv` has to be restored from the backup after parsing the
command-line: another parse is now possible by another client.


### API

#### The Interface

The `API` module is the heart of the preprocessor. Perhaps it is best
to start from its interface. The type of a preprocessor is

```
type 'src preprocessor = State.config -> 'src -> result
```

Clearly, the type parameter `'src` is the kind of input. The type
`State.config` is an object type gathering information about the input
and how the preprocessor is parameterised by the user. See section
[The State](#the-state). It could be that this information comes from
the module `CLI` if building the standalone preprocessor, but it could
come from the LIGO compiler, which uses the preprocessor as a library
and has its own command-line interface. The type `result` is

```
type result = (success, Buffer.t option * message) Stdlib.result
```

In case of error, the preprocessor (as a function) returns an error
message and an optional string buffer. That buffer contains the output
up to when the error occurred. This can be used to debug the
preprocessor. In case of success,

```
type module_deps = (file_path * module_name) list
type success     = Buffer.t * module_deps
```

the preprocessor returns a string buffer containing the output and a
list of module dependencies, directly gathered from the `#import`
directives.

Finally, different preprocessing functions are exported, according to
the type parameter `'src'`:

```
val from_lexbuf  : Lexing.lexbuf preprocessor
val from_channel : in_channel    preprocessor
val from_string  : string        preprocessor
val from_file    : file_path     preprocessor
```

#### The State

Let us turn our focus now on the implementation. We shall not explain
how `ocamllex` works. For that, please refer to its
[official documentation](https://ocaml.org/manual/lexyacc.html).

First of all, we decided to have one automaton. A preprocessor has
basically two fundamental states: at a given position in the input,
either the current character is copied to the output, or it is
discarded. This fact could have led to two automata, each state having
a dual in the other (copy versus skip). We decided not to do that and
only have one automaton, and this is why we have the type `mode`:

```
type mode = Copy | Skip
```

The mode will be threaded along the states of the automaton, which
will be therefore not duplicated.

In the same vein, checking the well-formedness of conditional
directives require to remember what kind of clause is valid at any
time. For example, when finding an `#elif` directive, it can only be
correct if either the previous conditional directive was another
`#elif` of an `#if`. This could be checked by an automaton that
duplicate its states according to what is valid (that is, was read
before). Just like with the modes above, we decided to have unique
states and instead keep track of the syntax of the conditional
directive with an argument to the scanner, of type `trace`:

```
type cond  = If of mode | Elif of mode | Else
type trace = cond list
```

It is a stack of previously encountered conditional directives, and
their modes (because even if they occur in regions of the input that
are skipped, they have to be well formed).

Already, we have seen two kind of information (modes, conditionals)
that need to be threaded along the states of the automaton. There is
actually more that needs threading, all gathered in the
type `State.t`:

```
type state = {
  config : config;
  env    : E_AST.Env.t;
  mode   : mode;
  trace  : trace;
  out    : Buffer.t;
  chans  : in_channel list;
  incl   : file_path list;
  import : (file_path * module_name) list
}
```

We find the fields `config`, `mode` and `trace` we considered earlier,
but also additional information.

  * The field `env` records the symbols defined by `#define` and not
    undefined by `#undef`. It acts as a value environment when
    computing the Boolean expressions of `#if` and `#elif`.

  * The field `out` is the output buffer.

  * The field `chans` is the list of input channels opened when
    processing an `#include` directive, in order to close them when we
    are done, and thus avoid a memory leak (channels are not collected
    by the OCaml collector).

  * The field `incl` is isomorphic to the file system path to the
    current input file, and it is changed to that of any included
    file. More precisely, its a stack on top of which directories are
    pushed by means of `API.push_dir`, and from which a path can be
    obtained back `API.mk_path`.

  * The field `import` is the list of modules from their defining
    file, as given by the `#import`

The entry point in the automaton is `API.scan`:

```
rule scan state = parse

```

We can see the parameter `state` that will be threaded along the
scanning rules.

#### Copying

There are two functions to copy input characters to the output buffer.
The first is `API.copy`:

```
let copy state buffer =
  Buffer.add_string state.out (Lexing.lexeme buffer)
```
and the other is `proc_nl` ("process newline character"):

```
let proc_nl state buffer =
  Lexing.new_line buffer; copy state buffer
```

**It is necessary to call `proc_nl` when copying newline characters.**
Remember too that newline characters are always copied, independent of
the mode.

#### Scanning Conditional Directives


When an `#if` directive is found, the trace is extended by calling

```
extend (If state.mode) state region
```

during the evaluation of which the syntactic validity of having
encountered an `#if` is checked (for example, it would be invalid had
an `#elif` been last read). Note that the current mode is stored in
the trace with the current directive --- that mode may be later
restored (see below for some examples). Moreover, the directive would
be deemed invalid if its current position within the line (that is,
its offset) were not preceded by blanks or nothing, otherwise the
function `API.expr` is called to scan the Boolean expression
associated with the `#if`: if it evaluates to `true`, then the
resulting mode is `Copy`, meaning that we may copy what follows,
otherwise we skip it --- the actual decision depending on the current
mode. That new mode is used if we were in copy mode, and the offset is
reset to the start of a new line (as we read a new line in `expr`);
otherwise we were in skipping mode and the value of the conditional
expression must be ignored (but not its syntax), and we continue
skipping the input.

When an `#else` is matched, the trace is extended by calling
`API.extend` with `Else` as an argument:

```
extend Else state region
```

and then the rest of the line is scanned and discarded by means of
the rule `skip_line`:

```
and skip_line state = parse
  nl     { proc_nl state lexbuf   }
| eof    { rollback lexbuf        }
| _      { skip_line state lexbuf }
```

(Keep in mind that newline characters are always copied.) If we were
in copy mode, the new mode toggles to skipping mode; otherwise, the
trace is searched for the last encountered `#if` of `#elif` and the
associated mode is restored.

The case of `#elif` is the result of the fusion (in the technical
sense of functional programming) of the code for dealing with an
`#else` followed by an `#if`.

When an `#endif` is matched, the trace is reduced, that is, all
conditional directives are popped until an `If mode` is found and its
`mode` is restored as the current mode.

Consider the following four cases, where the modes (`Copy`/`Skip`) are
located between the lines:

```
                    Copy ────┐                          Copy ────┐
   #if true                  │       #if true                    │
                    Copy     │                          Copy     │
   #else                     │       #else                       │
                ┌── Skip ──┐ │                      ┌── Skip ──┐ │
     #if true   │          │ │         #if false    │          │ │
                │   Skip   │ │                      │   Skip   │ │
     #else      │          │ │         #else        │          │ │
                └─> Skip   │ │                      └─> Skip   │ │
     #endif                │ │         #endif                  │ │
                    Skip <─┘ │                          Skip <─┘ │
   #endif                    │       #endif                      │
                    Copy <───┘                          Copy <───┘


                ┌── Copy ────┐                          Copy ──┬─┐
   #if false    │            │       #if false                 │ │
                │   Skip     │                          Skip   │ │
   #else        │            │       #else                     │ │
                └─> Copy ──┐ │                    ┌─┬── Copy <─┘ │
     #if true              │ │         #if false  │ │            │
                    Copy   │ │                    │ │   Skip     │
     #else                 │ │         #else      │ │            │
                    Skip   │ │                    │ └─> Copy     │
     #endif                │ │         #endif     │              │
                    Copy <─┘ │                    └───> Copy     │
   #endif                    │       #endif                      │
                    Copy <───┘                          Copy <───┘
```

The following four cases feature `#elif`. Note that we put between
brackets the mode saved for the `#elif`, which is sometimes restored
later.

```
                    Copy ──┐                            Copy ──┐
   #if true                │         #if true                  │
                    Copy   │                            Copy   │
   #elif true   ┌──[Skip]  │         #elif false    ┌──[Skip]  │
                │   Skip   │                        │   Skip   │
   #else        │          │         #else          │          │
                └─> Skip   │                        └─> Skip   │
   #endif                  │         #endif                    │
                    Copy <─┘                            Copy <─┘


                ┌── Copy ──┬─┐                      ┌── Copy ────┐
   #if false    │          │ │       #if false      │            │
                │   Skip   │ │                      │   Skip     │
   #elif true   └─>[Copy]  │ │       #elif false    └─>[Copy]──┐ │
                    Copy <─┘ │                          Skip   │ │
   #else                     │       #else                     │ │
                    Skip     │                          Copy <─┘ │
   #endif                    │       #endif                      │
                    Copy <───┘                          Copy <───┘

```

Note how `#elif` indeed behaves like an `#else` followed by an `#if`,
and the mode stored with the data constructor `Elif` corresponds to
the mode before the virtual `#if`.

#### Scanning File Inclusions

For a step-by-step explanation of how file inclusion works, please
refer to the source code.

#### Rollback

Across the semantic actions of the `ocamllex` specification `API.mll`,
we see calls to the function `rollback`:

```
let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}
```

That function restores the logical view of lexer engine over the
lexing buffer so that it would appear that its contents has not been
read. When called in a semantic action, it applies to the lexing
buffer that was matched by the corresponding regular expression.

##### Scanning Comments and Strings

As we saw in the section about
[preprocessing strings and comments](#preprocessing-strings-and-comments),
several combinations of block and line comments are possible. We also
saw above how the type `API.config`, in the section
[The Interface](#the-interface), gathers parametric information about
the behaviour of the preprocessor. In particular, we saw the signature
`COMMENTS` in the section [CLI](#cli) signature that gathers the
comment opening and closing markers from the client's perspective. It
is therefore important that what the client requests is actually
possible according to the regular expressions.

The regular expression `block_comment_openings` in `API.mll`:

```
let block_comment_openings =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening
| michelson_block_comment_opening
```

matches all the statically possible opening markers for block
comments.

The regular expression `line_comments`:

```
let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment
| michelson_line_comment
```

gathers the valid opening markers for line comments.

After matching all possible block comment openings
(*block_comments_openings*), we need to check whether the matched
opening is the one requested by the client:

```
| block_comment_openings {
    let lexeme = Lexing.lexeme lexbuf in
    match state.config#block with
```

If not, we [rollback](#rollback) the lexing buffer state and scan again
the characters that were matched (`scan_n_char n state lexbuf`), and
then resume scanning with `scan`:

```
    | Some _ | None ->
        let n = String.length lexeme in
          begin
            rollback lexbuf;
            assert (n > 0);
            scan (scan_n_char n state lexbuf) lexbuf
          end }
```

For example, if the client specifies that block comments start with
the sequence `(*` and we matched `/*`, we cannot consider the string
`/*` as a whole (a comment opening), but, instead as a normal
input. It has length `2`, so `scan_n_char 2` will scan `2` characters
without trying to match a comment opening.

If the matched comment opening is the one expected by the client,
then, if in copy mode, the opening is copied and a scanning rule is
called, depending on the kind of comment: `in_block` for block
comments, or `in_line` for line comments.

If we consider the definition of `in_block`, we see that it tries to
match three regular expressions on the lexing buffer:

  1. a string delimiter (it is an opening, but opening and closing are
     expected to be the same);

  2. a block comment opening;

  3. a block comment closing.

Actually, string delimiters and block comment opening share the same
semantic action because comments can contain strings and strings
comments. The idea here is to scan a string or a sub-comment, or to
treat the matched string as regular comment contents. Since comments
can be nested, we need, in case of error, like an open comment, to
refer the user to the opening of the comment. This is the purpose of
the `opening` parameter:

```
and in_block block opening state = parse
```


## PreprocMainGen

The module `PreprocMainGen` exports a functor which consumes a module
of signature `S` in `CLI` and returns a signature

```
    val check_cli  : unit -> unit
    val config     : API.config
    val preprocess : unit -> API.result
```

This signature packages the command-line options as defined by the
module `CLI` into an `API` version, both suitable for the standalone
preprocessor, and also for any library client, for example, the LIGO
compiler. This signature also provides two ready-made functions
`check_cli` and `preprocess` to check the command-line options and run
the preprocessor (on the input specified in the argument `CLI`),
respectively.

### PreprocMain

The module `PreprocMain` is only used as the entry point of the
standalone preprocessor, that is, `PreprocMain.exe`, if using `dune`,
or `PreprocMain.byte`, if using the Makefile. As such, it assumes that
there are no comments and no strings, so it may be run on all texts
that are not programming languages, to be tested.

## Maintaining the Preprocessor

This section explains how to perform some maintenance tasks aiming at
adding new features, but, whatever you modify in this directory, you
should re-read this file to ensure that the quoted code is current.

### Adding a Command-Line Option

When using the standalone preprocessor, we may want to add
[another command-line option](#the-standalone-preprocessor), for
example to dump debug information. For that purpose, the module `CLI`
needs to be considered. The new option needs to be added to the
signature `CLI.S`. If it is a debug option, it should not lead to an
error by being incompatible with other options, therefore there is not
need to extend the type `status` and the variant `` `Done`` will
do. The function [make_help] needs to be emended so `--help` displays
the new option. If it is for debugging, the description could be `For
debugging.` or `Not documented.` A global reference needs to be
created, which will hold the OCaml value corresponding to the option,
and it must be initialised with a default value, so an `option` may be
handy sometimes. See for example the reference `input`. If your option
is without arguments, add it to the value `opt_wo_arg`, like
`--show-pp` is added, for instance. If it has an argument, then add it
to `opt_with_arg`, like `-I`. Then re-export the OCaml value without a
reference, for example see

```
let dirs = !dirs
```
to comply with the signature `CLI.S`, where no reference escapes to
the client. You may want the new option to be debugged itself, by
adding a string to the value `options`:

```
      let options = [
        "CLI options";
        sprintf "input      = %s" (string_of_opt (fun x -> x) !input);
        sprintf "dirs       = %s" string_of_dirs;
        sprintf "show-pp    = %b" show_pp;
        sprintf "columns    = %b" (not offsets)
      ] in
```

If the new option could cause new errors to occur, then the type
`status` may have to be extended with another variant. Those variants
are polymorphic because they enable such an extension.

### Adding a Preprocessing Directive and/or Errors

It is more likely that you may want to add a new preprocessing
directive. In that case, you need first to think of its arguments and
ask yourself: How many? Any optional? Of what OCaml type? Can it fail?

The first step is to insert the name of the directive in the list
`directives`, in alphabetic order. Next, you need to add the
corresponding case to the pattern matching `match id with`. If the new
directive needs an argument, you can follow the example of another
directive that has a similar one. For example, if a string is
expected, you might considier how `#include` works. Anyway, the
general idea is that you want an error if the string is not valid or
missing (assuming it is not optional here). That is why you need a
scanning rule for the arguments. This is why, for instance, the
handling of `#include` needs calling `scan_include`:

```
        and reg, incl_file = scan_include state lexbuf in
```

and `#import` features a call to `scan_import`, like so:

```
        let reg, import_file, imported_module = scan_import state lexbuf in
```

In those rules, always have a catch-all clause and, if the argument is
the last, a clause matching `eof`, like so:

```
and new_rule state = parse
  ...
| eof { ... }  (* If parsing the last argument *)
| _   { ... }
```

Assuming the new rule might raise an error, you need to add it to the
type `Error.t`, both the implementation and the interface. You need
then to add a case for it in `Error.to_string`. In the semantic
actions of `new_rule`, **always** fail by calling either the function
`fail` or `stop`. In particular, _do not raise your own exception or
even the local exception `Error`_. That exception should not escape
the module `API`. It is catched when calling the entry point of the
lexer, in function `from_lexbuf`:

```
  match preproc state buffer with
    state ->
      List.iter close_in state.chans;
      Stdlib.Ok (state.out, state.import)
  | exception Error (buffer, msg) ->
      Stdlib.Error (Some buffer, msg)
```

and in the utility function `apply`:

```
let apply transform region state =
  match transform state with
    Stdlib.Ok state  -> state
  | Stdlib.Error err -> fail state region err
```

The internal exception `Error` is only raise by the function `fail`
and `expr`, which parses the Boolean expressions of directives `#if`
and `#elif`.

If the new directive is expected to gather information during the
preprocessing, you then need to extend the type `State.t` with
dedicated accumulators, and add functions to `State` to operate on
those accumulators. See the simplest examples, like `push_import`. The
rationale for modules `Error` and `State` is to limit the clutter of
`API`, which is the heart of the preprocessor. When the preprocessor
finished scanning the input, you need a way to export the new
information. Consider then the module `API` and the type `result`
there. At least, you need to augment the type `success` to return the
data gathered by the new directive in case of success, but you might
also return it in case of error, in which case you need to augment the
type `error`.

If the new directive opens input channels, make sure to register them
(push them) in the field `chans` of the `state`, so they are closed
and no memory leak occurs (see function `from_lexbuf`, which is called
by all the `from_` exported functions).

### Adding a New Kind of Comment

If you use the preprocessor for a language whose comments follow a
convention that is different from the one already implemented in the
preprocessor, here is what to do. First, create your own combination
of block and line comment. Perhaps it is useful you look at how the
convention for Michelson is implemented:

```
let michelson_block_comment_opening = "/*"
let michelson_block_comment_closing = "*/"
let michelson_line_comment          = "#"
```

Then

```
let block_comment_openings =
 ...
| michelson_block_comment_opening
```

and

```
let block_comment_closings =
  ...
| michelson_block_comment_closing
```

and

```
let line_comments =
  ...
| michelson_line_comment
```

That is all, as far as the library is concerned. The caller needs to
be made aware of the new convention in the documentation, so values of
the type `State.config` are valid (see methods `block` and `line`).

### Adding a New Kind of String

The preprocessor is aware of strings as being enclosed between double
quotes. If you consider adding a new kind of string, for example,
enclosed between simple quotes, a substantial change is needed. Please
talk to the maintainer.
