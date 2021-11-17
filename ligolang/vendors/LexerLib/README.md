# The Lexer Library

A lexer is a tool that reads a text file expected to be made of
_lexemes_, and returns abstract versions of them, called _tokens_, for
a parser to consume.

The current lexer library supports the making of UTF8-aware lexers for
programming languages, like LIGO, by having the library's client
provide a lexer for the tokens as a callback: the library takes care
of the markup (strings and comments), and calls back the client's
lexer for the tokens. In the case of LIGO, this enables sharing of the
boilerplate between the different concrete syntaxes.

This lexer library also offers a one or two-token window for the parser
in case of a parse error, so better error messages can be printed.

This lexer library can also be used to support tools other than
parsers, like pretty-printers and style checkers, as done in the LIGO
compiler suite, because it recognises the markup and the tokens, that
is, all lexical units.

It is designed so it can be easily used with the preprocessor library
also shipped with LIGO.

## Contents

This directory is located in the `vendors` directory of the repository
of LIGO, here `ligo/vendors`. As such, it is distributed with the LIGO
compiler which uses it as a library. The directory
`ligo/vendors/LexerLib` should list the following files:

```
LexerLib
├── API.ml
├── API.mli
├── Client.mli
├── CLI.ml
├── CLI.mli
├── Core.mli
├── Core.mll
├── Directive.ml
├── Directive.mli
├── dune
├── dune-project
├── Error.ml
├── Error.mli
├── LexerLib.opam
├── LICENSE
├── Markup.ml
├── Markup.mli
├── README.md
├── State.ml
├── State.mli
├── Thread.ml
├── Thread.mli
└── Unit.mli
```

Here is a short description of some of those files and OCaml modules:

  * The OCaml module `Core` is the heart of the lexer library. It can
    be considered as the low-level lexer engine, with lots of bells
    and whistles. It is a private module, as far as `dune` is
    concerned, that is, not to be exported by the library.

  * The module `API` packages types, functions and modules for the
    client, based on `Core`. It is, in a way, a simplified view of
    `Core`, with some specific uses in mind. This is a public module.

  * The module `CLI` deals with command-line options for building a
    standalone lexer, and also exports data structures about the
    configuration meant for the library client, for example, the LIGO
    compiler. This is a public module.

  * The module `Error` defines the errors that the lexer library can
    emit. It is a private module.

  * The module `State` defines types and related functions used for
    lexing. In particular, the state is a data structure threaded
    along the calls to scanning rules and which hold a high-level view
    of the process, including the source and the lexing buffer. It is
    a public module.

  * The module `Directive` defines the abstract representation and
    printers for the preprocessing directives. Indeed, the lexer is
    designed so it can be used with the preprocessor library, in which
    case,
    [linemarkers](https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html)
    can be found in the input of the lexer. It is a public module.

  * The module `Markup` defines abstract representations for the sorts
    of markup recognised by the lexer: tabulations, blanks, newline
    characters, comments etc. It is a public module.

  * The interface `Unit` defines a type for lexical units. It is public.

  * The interface `Client` defines the signature expected from the
    client of the library to use the `API`. It is of course public.

  * The `LICENSE` file must contain the MIT license.

  * The file `README.md` is the present file.

## Interfaces

### The CLI Interface

The file `CLI.mli` shines a light on the design of `CLI.mli` in the
preprocessor library, in the directory `vendors/Preprocessor`. The
present module `CLI` is meant to be composed with the one of the
preprocessor library.

The `CLI` of the lexer library re-exports the CLI of the preprocessor
as module `Preprocessor_CLI`:

```
module type S =
  sig
    module Preprocessor_CLI : Preprocessor.CLI.S

    val preprocess : bool

    val mode : [`Byte | `Point]

    val command : [`Copy | `Units | `Tokens] option

    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string (* Two conflicting options *)
    ]

    val status : status
  end
```

This is a design pattern we will see at work again in the parser
library `ParserLib`:

  1. Include and re-export the previous CLI in the compiler pipeline.

  2. Extend the type `status` with any new error that may arise by
     running this CLI.

  3. Export a value `status` of that type.

  4. Add extra fields (e.g., `preprocess`, `mode`, `command`).

This pattern makes it possible to accumulate CLIs from one pass of the
compiler to the next: the command-line options of the previous stage
are added to the current ones, and only the latter need defining.

If the value `mode` is `` `Byte``, then the unit in which source
positions and regions are expressed in messages is the byte. If ``
`Point``, the unit is unicode points.

The value `command` denotes some possible behaviours of the
compiler. The constructors are

  * `` `Copy``: the lexemes of tokens and markup will be printed to
    standard output, with the expectation of a perfect match with the
    input file;

  * `` `Units``: the tokens and markup will be printed to standard
    output, that is, the abstract representation of the concrete
    lexical syntax;

  * `` `Tokens``: the tokens only will be printed.

### The Client Interface

The module `Client` has no implementation because it only specifies
types that the client of the lexer library needs to satisfy. The main
requirement is specified by the type `scanner` in `Client.mli`:

```
type 'token scanner =
  'token State.t ->
  Lexing.lexbuf ->
  ('token * 'token State.t, message) Stdlib.result
```

A scanner of type `'token scanner` is a bit like a generic `ocamllex`
parse rule:`

```
rule scanner state = parse
...
```

It takes the current state and lexing buffer and returns either an
error message or a token and a new state (the lexing buffer is updated
imperatively, so there is no need to return it). The tell-tale that
this about the client is that a `'token` is returned. The value
`client` of the signature `LEXER` is the fundamental scanner (lexer)
from which all those exported by the signature `S` of `API.Make` are
made, that is, either lexers that return the full list of lexical
units or tokens, and this from a variety of sources. See below
[the functor Make](#the-api-interface).

Continuing with the client-side, we further read in `Client.mli`:

```
type 'token cut =
  Thread.t * 'token State.t -> 'token * 'token State.t

type 'token client = <
  mk_string                : 'token cut;
  callback                 : 'token scanner;
  is_string_delimiter : char -> bool
>
```

The type `cut` is the type of a function that takes a
[thread](#type-thread) and returns the corresponding token, which can
only be a string (a comment is markup, therefore can only be made by
the library, not the client lexer). The state is updated in the
process, so it is given as an argument and its new version is
returned.

The type `client` gathers all the information the client lexer needs
to provide:

  * A method `mk_string` that makes a string from a thread (it must be
    provided by the client because only the client knows the
    implementation of tokens, and that can be ascertained by
    considering that they are type parameters in `Core`.).

  * A method `callback` which is a lexer/scanner (both words are
    equivalent in this documentation) for tokens other than strings
    (as `mk_string` takes care of those).

  * A method `is_string_delimiter` which is a predicate telling
    whether the character it is given delimits a string. This enables
    to have different string delimiters for different client lexers.

### The API Interface

The interface `API.mli` is the best place to start to understand the
architecture and client-side features offered by the lexer
library. It is made of the following sections.

  1. A functor `Make` that takes a client lexer (that is, scanning
     tokens) and returns a module declining many kinds of lexers,
     including specialisation from different sources to different
     results.

  2. A function `reset` that sets different components of a lexing
     buffer.

Let us start with the client lexer.

The first section of the interface of module `API` deals with what the
client of the library needs to do to obtain a: the signature `LEXER`:

```
module type LEXER =
  sig
    type token

    val client : token Client.t
  end
```

See the section about [the Client Interface](#the-client-interface).

The functor `Make` is one of three tightly coupled components:

  1. a signature `LEXER`, which we just explained above in the
     section about [the client-side](#the-client-side);

  2. a signature `S`;

  3. a functor `Make` from `LEXER` to `S`.

The signature `S` is best construed as a simplification and
specialisation of the interface of `Core`, which is a private module,
therefore cannot be accessed by the clients of this library. We shall
explain `Core` in section about
[the Core interface](#the-core-interface).

The purpose of the functor `Make` is to take that low-level
representation of a lexer by instantiating `Core` with a module of
signature `LEXER`, and produce a high-level module that enables the
library's client to build a standalone lexer or to integrate it in the
LIGO compiler. It would be overly complicated for casual users to use
`Core` directly, but not exporting `Core` has a consequence: _the
lexers actually exported by `API` return all the tokens or lexical
units in their input_. This is not what is expected by parser
generators like `menhir`. The LIGO compiler uses `menhir`, and we
encourage you to see how the present lexer library is interfaced with
the parsers. (Basically, a buffer is used to store all the tokens that
the parser is likely to request, and the new lexer provides them one
by one.)

Usual lexers are only expected to produce tokens, that is, words in
the input that are relevant to a parser, but this library also
features the production of _lexical units_, which is a superset of
tokens: tokens and markup (see module `Markup`). The rationale for
this unusually rich information is to enable tools other than parsers
to have access to the maximum of details about the input source. Such
a tool could be a pretty-printer, or a style checker, as found in the
LIGO compiler suite (it is not just a compiler).

Let us consider the type of lexers, as found in the signature `S` in
`API`:

```
    type ('src,'dst) lexer =
      token State.config -> 'src -> ('dst, message) Stdlib.result
```

The first parameter of this functional type is `token State.config`,
that is, an object type gathering generic information about the input,
like whether comments are expected and, if so, what kind, also whether
the input was a file or `stdin`, etc. The second parameter is the
source read by the lexer, and the successful result is the type
parameter `'dst`.

The module `Tokens` in `S` exports the following set of lexers:

```
    module Tokens :
      sig
        val from_lexbuf  : (Lexing.lexbuf, token list) lexer
        val from_channel : (in_channel,    token list) lexer
        val from_string  : (string,        token list) lexer
        val from_buffer  : (Buffer.t,      token list) lexer
        val from_file    : (file_path,     token list) lexer
      end
```

They have in common the production of all the tokens of the input, if
there are no errors, of course. This is useful when building
standalone lexers, for testing purposes for instance.

Finally, the module `LexUnits` in `S` is more general than `Tokens`,
in the sense that its lexers return _lexical units_ rather than only
tokens. Lexical units consists in tokens and markup.

```
    module LexUnits :
      sig
        type nonrec 'src lexer = ('src, token State.lex_unit list) lexer

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end
  end
```

Finally, the last section of the API interface exports the function
`API.reset`, which resets either the file name, the current line
number, or the horizontal offset in a lexing buffer. This function is
useful when lexing a file that has been previously preprocessed, in
which case the argument labelled `file` is the name of the file that
was preprocessed, _not_ the preprocessed file (of which the user is
not normally aware). This is mostly a technical function, that
packages together relatively simple updates to the field `lex_curr_p`
of
[the record of type `Lexing.lexbuf`](https://ocaml.org/api/Lexing.html).

```
type file_path = string

val reset :
  ?file:file_path ->
  ?line:int ->
  ?offset:int ->
  Lexing.lexbuf ->
  unit
```

### The Thread Interface

When scanning structured constructs, like strings and comments, we
need to keep the region of the opening symbol, like a double quote for
strings, the marker `//` or `(*`, in order to report any error in them
more precisely. Since `ocamllex` is byte-oriented, we need to store
the parsed bytes as characters in an accumulator so, when are done
scanning the compound, it is easy to build the string making up the
structured construct. The data structure gathering everything needed
for this goal is called here a _thread_ (because it is threaded along
calls) and is defined in module `Thread` as follows:

```
type thread = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread
>
```

The fields are explained as follows:

  * The field `opening` holds the location of the opening marker of a
    string or a comment.

  * The field `length` is the length of the field `acc`.

  * The field `acc` is a stack made of the characters of the string or
    comment being recognised (the characters are therefore reversed).

  * The field `to_string` extracts the string or comment, by relying
    on the fields `acc` and `length`.

  * The methods `push_char` and `push_string` push a recognised
    character, respectively string, onto the accumulator.

  * The method `set_opening` set the field `opening` to a given
    value. It is used when scanning nested comments: when inside a
    comment and finding the start of another one, the opening of the
    current one is saved, the opening of the new one is set with
    `set_opening` and scanning is resumed until the nested comment is
    finished and we can restore the original opening.

Threads are used in the module `State`.

### The Unit Interface

Lexical units are produced by the module `LexUnits` exported by
`API.S`, but they are defined in their own module, like so:

We mentioned several times that this lexer library can supply lexical
units, tokens and preprocessing directives (e.g. linemarkers). More
precisely, this means:

```
type 'token lex_unit = [
  `Token     of 'token
| `Markup    of Markup.t
| `Directive of Directive.t
]
```

### The State Interface

Beyond producing lexical units (including tokens), the result of
lexing is a _state_. The type `State.t` represents the abstract
logical state of the lexing engine, that is, a value which is threaded
during scanning and which denotes useful, high-level information
beyond what the type `Lexing.lexbuf` in the standard library already
provides for all generic lexers. We call it a "logical state" because
the lexing buffer itself has a "physical state" defined by the type
`Lexing.lexbuf`.

Tokens are the smallest units used by the parser to build the abstract
syntax tree. Lexical units include tokens, but also markup
(whitespace, comments) and preprocessing directives that were
generated by the preprocessor, like linemarkers after file
inclusions. See the section on
[the Unit interface](#the-unit-interface). (Other directives coming into the
preprocessor would mean they are invalid, as the preprocessor does not
raise an error in that case.)

Type `State.config` gathers information about the input, about what to
do, about how to display error messages, and how to print tokens:

```
type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type command = [`Copy | `Units | `Tokens] option

type 'token config = <
  block     : block_comment option;
  line      : line_comment option;
  input     : file_path option;
  offsets   : bool;
  mode      : [`Byte | `Point];
  command   : command;
  is_eof    : 'token -> bool;
  to_region : 'token -> Region.t;
  to_lexeme : 'token -> string;
  to_string : offsets:bool -> [`Byte | `Point] -> 'token -> string
>
```

Let go through all the fields:

  * The value for the field `block` is `None` if the lexer should not
    look for multi-line comments, also called here _block
    comments_. If block comments are to be expected, the type
    `block_comment` gathers how they start and how they close.

  * The field `line` is the equivalent of `block` for one-line
    comments, here called _line comments_, and holds the string that
    act as an opening marker, if any.

  * The value of the field `input` is `None` if the input is
    `stdin`. Otherwise, it holds a filesystem path to the input file.

  * The value for the field `offsets` is `true` to have any error
    message report the location in the input source in terms of
    horizontal offset, à la Emacs, instead of column number (`false`,
    like Vim).

  * The field `mode` records whether error messages should report
    locations in the source assuming UTF-8 codepoints or only bytes
    (characters). UTF-8 is only recognised as such in comments, so
    choosing `` `Point`` versus `` `Byte`` has an impact only on
    errors happening after a UTF-8 comment.

  * The field `command` registers an optional action to undertake while
    the lexer is running: ``Some `Copy`` will reproduce the input to
    the output from the lexical units, ``Some `Units`` will print out
    the lexical units, and ``Some `Tokens`` will print out only the
    tokens. This is for debugging the lexer.

  * The remaining fields are functions extracting information from
    tokens or converting them, mostly for printing purposes.

The type of the tokens is a parameter, because it is defined by the
client of the library.

Then there is the type `window`:

```
type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>
```

An object of this type is used in case of parse error. When a parser
consumes the tokens produced by the client of this library, it is fed
a window of the last or two last tokens, so the error message is more
precise.

We can now look at the mouthful

```
type 'token state = <
  config        : 'token config;
  window        : 'token window option;
  pos           : Pos.t;
  set_pos       : Pos.t -> 'token state;
  slide_window  : 'token -> 'token state;
  sync          : Lexing.lexbuf -> 'token sync;
  decoder       : Uutf.decoder;
  supply        : Bytes.t -> int -> int -> unit;
  mk_line       :      Thread.t -> Markup.t;
  mk_block      :      Thread.t -> Markup.t;
  mk_newline    : Lexing.lexbuf -> Markup.t * 'token state;
  mk_space      : Lexing.lexbuf -> Markup.t * 'token state;
  mk_tabs       : Lexing.lexbuf -> Markup.t * 'token state;
  mk_bom        : Lexing.lexbuf -> Markup.t * 'token state;
  mk_linemarker : line:string ->
                  file:string ->
                  ?flag:char ->
                  Lexing.lexbuf ->
                  Directive.t * 'token state
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}
```

The fields and methods are as follows.

  * The field `config` is explained by the section on
    [the type config](#type-config).

  * The field `window` is explained above in this section.

  * The field `pos` is the current position in the file.

  * The method `set_pos` sets the current position in the file. This
    is useful when dealing with linemarkers, or newline characters. In
    other cases, the field `pos` is updated by the method
    `sync`. Indeed, the position is not always updated after a single
    character has been matched: that depends on the regular expression
    that matched the lexing buffer, and whether we decide to perform a
    rollback by calling `rollback` or not.

  * The method `slide_window` updates the field `window` when new
    tokens are recognised.

  * The method `sync` updates the state after a regular expression
    matched. It updates the current position in accordance with the
    contents of the lexing buffer, more precisely, depending on the
    length of the string which has just been recognised by the
    scanner: that length is used as a positive offset to the current
    column. The return type `sync` is a record meant to contain the
    `region` of the lexeme in the input source, the `lexeme` itself
    and the updated `state`. The method `sync` must be called in the
    semantic action with the lexing buffer that was matched by the
    regular expression.

  * The field `decoder` and method `supply` offer the support needed
    for the lexing of UTF-8 encoded characters in comments (the only
    place where they are allowed in our input languages). The former
    is the decoder proper and the latter is the effectful function
    that takes a byte, a start index and a length and feed it to
    `decoder`. See the documentation of the third-party library
    [Uutf](https://github.com/dbuenzli/uutf).

  * The remaining methods have names of starting with `mk_` followed
    by the kind of lexical unit (not a token) they build: `mk_line`
    for line comments, `mk_block` for block comments, `mk_newline` for
    newline characters, `mk_space` for blank space, `mk_tabs` for
    tabulations and `mk_bom` for the UTF-8
    [Byte-Ordered Mark](https://en.wikipedia.org/wiki/Byte_order_mark). Note
    how they all return a new state, besides the lexical unit.

A few words about the method `mk_linemarker`:

```
  mk_linemarker : line:string ->
                  file:string ->
                  ?flag:char ->
                  Lexing.lexbuf ->
                  Directive.t * 'token state
```

Just as the methods of `state` whose name start with `mk_`, the method
`mk_linemarker` updates the state after a linemarker has been
recognised, and thus returns a new state. Before describing the
parameters, let us recall that the form of a line marker is:

```
# <line> "<file name>" [<optional 1 or 2>]
```

So, the first parameter of `mk_linemarker` is the `<line>` specified
by the marker; the second is the `<file name>` specified by the
marker; the third is the optional flag `1` or `2` of the marker; the
last and fourth is the current lexing buffer. The lexical unit
returned is constructed using `Directive`.

Another remark about `mk_line` and `mk_block`: they types are
different from the other methods in `mk_`. Indeed they return a value
of type `Markup.t`, but no new state. There is a technical reason
which can only be understood by looking at
[the State Implementation](#the-state-implementation) and
[the Core Implementation](#the-core-implementation).

### The Core Interface

The module `Core` is the heart of the library and it is specified as
private in the file `dune`, meaning that it cannot be used directly by
the clients of the library. Instead, the clients must use the aptly
named module `API`. The following is therefore intended for
maintainers of the library.

Let us walk through the interface file `Core.mli` and explain some of
its values and types, starting with the types that we saw in the
section about [the API interface](#the-api-interface).

Let us move now to the data structure which represents the
feature-rich, parameterised lexer of `Core`:

```
type input =
  File    of file_path
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

type 'token instance = {
  input      : input;
  read_token : Lexing.lexbuf -> ('token, message) result;
  read_unit  : Lexing.lexbuf -> ('token lex_unit, message) result;
  lexbuf     : Lexing.lexbuf;
  close      : unit -> unit;
  window     : unit -> 'token window option
}
```

A look back at `API.mli` is useful here. The functions in the modules
`Tokens` and `LexUnits` are specialised lexers based on `read_token`
and `read_unit`, respectively, as we will see in the section about the
implementation of `Core`.

The type `input` is straightforward: it informs about the kind of
input. The type `instance` is worth explaining in more details.

  * The field `input` is obvious.

  * The field `read_token` is a lexer that returns one token or an
    error message, almost in the fashion expected by `menhir`.

  * The field `read_unit` is more general than `read_token`, as it
    returns one lexical unit per call, in case of success.

  * The field `lexbuf` is the current lexing buffer.

  * The method `close` closes the input channel, if any (this depends
    on the field `input`), to avoid memory leaks.

  * The method `window` returns an option window of the last and maybe
    penultimate token (for error messages).

Finally, the consider the function `open_stream`:

```
val open_stream :
  'token Client.t ->
  'token State.config ->
  input ->
  ('token instance, message) Stdlib.result
```

This is the function that makes a lexer `instance`. It is only used in
the implementation of `API`, for example:

```
    let from_file config path =
      Core.open_stream Lexer.client config (Core.File path)
```

## Implementations

We now cover the implementations.

### The CLI Implementation

It is important to compare the files `CLI.ml` from
`vendors/Preprocessor` and `vendors/LexerLib`, as the exercise will
reveal the rationale for their design. Please read the section `CLI`
of `vendors/Preprocessor/README.md`.

We already went through [the interface](#the-cli-interface), so let us
walk through now the functor `Make`.

  * In the `Preprocessor` library, the functor `Make` takes as
    argument a module of signature `COMMENTS`.

  * In the `LexerLib` library, the functor `Make` takes as argument a
    module of signature `PREPROCESSOR_CLI`, which is equal to the
    return signature `S` of `Make` in the preprocessor library.

This means that we can compose a call to `Make` from the preprocessor
library with a call to `Make` from `LexerLib`. We already saw that the
return signature `S` of `Make` in `LexerLib` includes a module of
signature `PREPROCESSOR_CLI`, that is to say, the CLI of `LexerLib`
relies on the CLI of the preprocessor to read its relevant
command-line options, then reads its own, and everything is exported
in a module of signature `S` (but not in a flat way: the CLI of the
preprocessor is a submodule). Let us look at the details now, starting
with the function `make_help`:

In the `Preprocessor` library, it starts like so:

```
    let make_help () : Buffer.t =
      let file   = Filename.basename Sys.argv.(0) in
      let buffer = Buffer.create 203 in
      let header =
        sprintf "Usage: %s [<option> ...] -- [<input>]\n\
                 where <input> is the source file (default: stdin),\n\
                 and each <option> (if any) is one of the following:\n"
                file
      and options = [
        "  -I <paths>       Inclusion paths (colon-separated)";
        "  -h, --help       This help";
        "  -v, --version    Commit hash on stdout";
        "      --cli        Print given options (debug)";
        "      --columns    Columns for source locations";
        "      --show-pp    Print result of preprocessing"
      ] in
```

whereas in the `LexerLib`:

```
    let make_help buffer : Buffer.t =
      let options = [
        "  -t, --tokens     Print tokens";
        "  -u, --units      Print lexical units";
        "  -c, --copy       Print lexemes and markup";
        "      --bytes      Bytes for source locations";
        "      --preprocess Run the preprocessor"
      ] in
```

We see that the latter has a parameter `buffer` and adds to it (that
is, the former buffer made by the `make_help` of the preprocessor)
only the options that are specific to lexing. Let us compare now the
type and value `status`.

In the `Preprocessor` library, we have:

```
    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    let status =
      try
        Getopt.parse_cmdline specs anonymous;
        `Done (* Default. Other values assigned below. *)
      with Getopt.Error msg -> `SyntaxError msg
```

and in `LexerLib`:

```
    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string
    ]

    let status = (Preprocessor_CLI.status :> status)
```

Therefore, we use explicit structural subtyping on polymorphic variant
types to initialise our `status` with that made by the CLI of the
preprocessor. Next:

```
    let status =
      try
        Getopt.parse_cmdline specs anonymous; status
      with Getopt.Error msg -> `SyntaxError msg
```

That is, we parse the command-line for options relevant to the lexer
(this is the second parse: the first was performed by the preprocessor
CLI). Next, we check that the new options do not conflict with each
other, making use now of the `` `Conflict`` polymorphic variant added
to `Preprocessor_CLI.status` (see above):

```
    (* Checking combinations of options *)

    let status, command =
      match copy, units, tokens with
        ...
      | true, true, _ -> `Conflict ("--copy", "--units"), None
      | true, _, true -> `Conflict ("--copy", "--tokens"), None
      | _, true, true -> `Conflict ("--units", "--tokens"), None

```

Finally, we update the status for the options that have been augmented
by the lexer library, that is: the help message, the list of the CLI
options and the version hash:

```
    let status =
      match status with
        `Help buffer  -> `Help (make_help buffer)
      | `CLI buffer   -> `CLI (make_cli buffer)
      | `Version _    -> `Version Version.version
      | _             -> status
```

### The API Implementation

Perhaps surprisingly, the API implementation is relatively simple, at
least simpler than the CLI implementation. We explain here salient
points of the implementation of the module `API`, whose interface was
presented [above](#the-api-interface). Perhaps the only point worth
mentioning is that almost all functions that are exported are defined
by means of the following:

```
    (* Generic lexer for all kinds of inputs *)

    let generic lexbuf_of config source =
      let buffer = Core.Buffer (lexbuf_of source) in
      Core.open_stream config ~scan:Lexer.scan buffer
```

or its result (a lexer instance of type `token Core.instance`),
the exception being

```
    let from_file config path =
      Core.open_stream Lexer.client config (Core.File path)
```

### The State Implementation

The function `State.make` creates the state threaded along the lexer
engine. It contains all methods to update the state. The most commonly
called is `sync`:

```
    method sync lexbuf : 'token sync =
      let lexeme = Lexing.lexeme lexbuf in
      let length = String.length lexeme
      and start  = pos in
      let stop   = start#shift_bytes length in
      let state  = {< pos = stop >}
      and region = Region.make ~start:pos ~stop
      in {region; lexeme; state}
```

As it names indicates, it is called early in the semantic actions when
it is clear that we do not want a rollback, therefore we "synchronise"
the logical state of `state` with that of the lexing buffer
`lexbuf`. In other words, the prefix of the lexing buffer that has
been matched by the corresponding regular expression is committed to
the state, as well as the current position in the source.

Another method worth mentioning perhaps is `mk_newline`:

```
    method mk_newline lexbuf =
      let ()     = Lexing.new_line lexbuf in
      let value  = Lexing.lexeme lexbuf in
      let start  = self#pos in
      let stop   = start#new_line value in
      let region = Region.make ~start ~stop in
      let markup = Markup.Newline Region.{region; value}
      in Markup markup, self#set_pos stop
```

Note how we call `Lexing.new_line` and `start#new_line`. Indeed, it is
of the utmost importance to call those two functions after recognising
an end-of-line character, so both the logical state and lexing buffer
register the change of lines in the input. See the scanning rule
`in_block` for an example, and, of course, `scan` itself.

There two methods of the object type `State.t` are similar in their
difference with the other in the way they process the state: `mk_line`
and `mk_block` do not produce a new state. For example:

```
    method mk_line thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      Markup.LineCom Region.{region; value}
```

This is because they only make use of their _thread_ argument.


### The Thread Implementation

The implementation of the module `Thread` is straightforward.

### The Core Implementation

The heart of the lexer library resides in the `ocamllex` specification
`Core.mll` and how, from a lexer for tokens provided by the client, a
lexer for the tokens _and the other lexical units_ is made. Actually,
how several lexers are made, depending on the kind of input.

In the section about the lexer instance, we find the definition of a
function `output_unit`. Its purpose is to print tokens, or all lexical
units, or lexemes onto an output channel, for debugging purposes. This
enables to see up to what lexical unit the lexing went before it
failed, either due to an internal error or an error in the input file.

The function `lexbuf_from_input` creates a lexing buffer from the
variety of possible inputs modelled by the type `input`. See
[the Core Interface](#the-core-interface). Two points of note:

  1. In the case of `Buffer` as an input, we check whether the input
     configuration `config` registers a file name: if so, the lexing
     buffer is patched with that name. (This has to be done manually.)

  2. In the case of `File` as an input, we call `reset ~file:path
     lexbuf` for the same reason.

#### Error Handling

The next section is about lexing errors and is worth detailing here.
As we saw in the description of
[the Core Interface](#the-core-interface), we do not export
exceptions. Nevertheless, it is convenient to use one exception in
case of error in the semantic actions of the implementation. That is:

```
(* Errors (NOT EXPORTED) *)

exception Error of string Region.reg
```

The client of the library provides a lexer for the tokens that is not
expected to raise exceptions. Instead, it returns a value of type
`Stdlib.result`. See the section about
[the Client Interface](#the-client-interface). We call the former
style "exception-raising style" and the latter "error-passing
style". We needs to convert lexers from one style to the other,
depending whether we are receiving a lexer from the client, or
exporting a lexer to the client. Internally, exception-raising style
is used, and, externally, error-passing style. Here are the two
converters:

```
(* Encoding a function call in exception-raising style (ERS) to
   error-passing style (EPS) *)

let lift scanner lexbuf =
  try Stdlib.Ok (scanner lexbuf) with
    Error msg -> Stdlib.Error msg

(* Decoding a function call in EPS to ERS *)

let drop scanner lexbuf =
  match scanner lexbuf with
    Stdlib.Ok state -> state
  | Error msg -> raise (Error msg)
```

The function `drop` is therefore called once, namely, in `mk_scan`:

```
let mk_scan (client: 'token Client.t) =
  let internal_client =
    object
      method mk_string = mk_token <@ client#mk_string

      method callback state =
        mk_token <@ (drop @@ client#callback state)

      method is_string_delimiter = client#is_string_delimiter
    end
  and first_call = ref true in
  fun state ->
    let scanner =
      if !first_call then (first_call := false; init) else scan
    in scanner internal_client state
```

That function converts the client lexer `client#callback` from
error-passing style to exception-raising style by calling

```
drop @@ client#callback state
```

The additional composition with `mk_token` is due to the client only
scanning for tokens, but `Core` uses `Unit.lex_unit`, so the
constructor `` `Token`` needs to wrap the tokens found by the client,
and this is exactly what `mk_token` does.

The converter from exception-raising style to error-passing style is
done at the end of the function `open_stream`, as expected:

```
  match lexbuf_from_input config input with
    Stdlib.Ok (lexbuf, close) ->
      let read_unit  = lift read_unit
      and read_token = lift read_token in
      Ok {read_unit; read_token; input; lexbuf; close; window}
  | Error _ as e -> e
```

Indeed, the type `instance` contains _two_ lexers: one for lexical
units, `read_unit`, and one for tokens only, `read_token`. Those need
lifting because the value of type `instance` they belong to is
exported back to the client of the library.

The maintainers of this lexer library should call the following
function `fail` instead of raising directly an `Error` exception:

```
let fail region error =
  let value = Error.to_string error in
  raise (Error Region.{value; region})
```

### The function `open_stream`

As we saw in the section about
[the Core Interface](#the-core-interface), the function `open_stream`
exported by the module `Core` is only used by `API` to instantiate the
sundry kinds of lexers it offers. The functions is made of four parts.

The first consists in defining some local variables of use in the
following parts, in particular a reference `state` of type `State.t
ref`. The reason for it is that `open_stream` returns lexers (fields
`read_token` and `read_unit` in the lexer instance of type `'token
instance`) of type:

```
  read_token : Lexing.lexbuf -> ('token, message) result;
  read_unit  : Lexing.lexbuf -> ('token Unit.t, message) result;
```

In other words, those lexers do not take a value of `State.t` as
input, and do not return a new state with the token or the lexical
unit. That is why the state (and the client), which is needed by the
lexing rules

```
rule scan client state = parse
```

have to be hidden in a reference in the scope of `read_token` and
`read_unit`. So the first part is

```
  let log       = output_unit config stdout
  ...
  let window () = !state#window in
```

Notice the first value, `log`, which is used for tracing the behaviour
of the lexers `read_token` and `read_unit`.

The second part is the definition of `read_unit`:

```
  let read_unit lexbuf =
    let unit, state' = scan !state lexbuf in
    let () = log unit in
    let () = state := state' in
    let () =
      match unit with
        `Token token -> state := !state#slide_window token
      | `Markup _ | `Directive _ -> ()
    in unit in
```

It is quite straightforward. Perhaps notice how the token window is
slided if the recognised lexical unit is actually a token.

The third part is the definition of `read_token`, which is simply
based on `read_unit`:

```
  let rec read_token lexbuf =
    match read_unit lexbuf with
                  `Token token -> token
    | `Markup _ | `Directive _ -> read_token lexbuf in
```

Simply, it skips any markup or directive, and returns the tokens
without the wrapping `` `Token``.

The fourth and last part of `open_stream` we already have seen above
when presenting the converters `lift` and `drop`, in the context of
[error handling](#error-handling):

```
  match lexbuf_from_input config input with
    Stdlib.Ok (lexbuf, close) ->
      let read_unit  = lift read_unit
      and read_token = lift read_token in
      Ok {read_unit; read_token; input; lexbuf; close; window}
  | Error _ as e -> e
```

A lexing buffer is made from the input and its configuration. In case
of success, we lift `read_unit` and `read_token` and we are done with
the fields of `'token instance`.

We should now turn our attention to the scanning rules (we will come
back to the header function `scan_utf8_wrap` and regular
expressions). Please not that, in `ocamllex`, those rules are called
_parsing rules_, as the keywords `rule` and `parse` show.

### The `scan` Parsing Rule

The main entry rule is `scan`, and its first line has already been
quoted several times already:

```
rule scan client state = parse
```

It shows that `scan` takes as argument a value of type `Client.t`
(which includes the client lexer as a callback), a value of type
`State.t` (the logical or high-level state, in opposition to the state
of the lexing buffer, which might be considered physical and
low-level, in our context), and the implicit lexing buffer of type
`Lexing.lexbuf`. The rule `scan` is associated with the initial state
in the underlying pushdown automaton. Because the lexer also has to
recognise structures, namely strings and comments, there are other
parsing rules which correspond to different states. The state of
`scan` (in terms of the automaton, not the parameter `state`) means
that we are _outside_ a string or a comment, or at the top level, for
example when we start scanning the input file.

The `scan` rule has five sections, or cases:

  1. markup (newline characters, whitespace, tabulations);

  2. strings,

  3. comments,

  4. linemarkers,

  5. tokens, including `eof` (end-of-file character).

In the case of markup and linemarkers, regular expressions are used to
completely characterise the category, because markup is simple and
linemarkers are only generated by the preprocessor library, so we do
not have to expect an invalid structure. The default case is also very
simple:

```
| eof | _ { rollback lexbuf;
            client#callback state lexbuf (* May raise exception [Error] *) }
```

Notice how a token other than `eof` is not actually recognised
here. Instead, the client lexer is called as a callback on a lexing
buffer that has been rolled back, that is, its state has been patched
so it appears as if no character was matched. (See function `rollback`
in the header of `Core.mll`.) The client callback is supposed to scan
for tokens, but it can of course fail, in which case it will return an
error that is then transformed into the exception `Error`, as we saw
in the section about [error handling](#error-handling).

Remain the cases of strings and comments. The latter is quite
complicated by the necessity of recognising UTF-8 encoded glyphs, so
let us start with strings. The program is as follows:

```
| '\'' | '"' as lexeme {
    if client#is_string_delimiter lexeme then
      let State.{region; state; _} = state#sync lexbuf in
      let thread = Thread.make region in
      in_string lexeme thread state lexbuf |> client#mk_string
    else (rollback lexbuf; client#callback state lexbuf) }
```

The regular expression matches characters that are possible delimiters
for strings, but whether those are actually actual delimiters depends
on the client, hence the conditional test on
`client#is_string_delimiter lexeme`.

  * If not a valid delimiter, the lexing buffer is rolled back and the
    client lexer is called back. Indeed, we assume that the characters
    delimiting strings, comments and linemarkers are all
    different. This can be a limitation in the case of a language like
    Michelson, whose comments start with `#`, like a linemarker.

  * If a valid string delimiter, we synchronise the logical state
    `state` with the physical state of the implicit lexing
    buffer. (See the section about
    [the State Interface](#the-state-interface).) Then a _thread_ is
    created (see the section on
    [the Thread Interface](#the-thread-interface)) which starts with
    the delimiter `lexeme`, and the specialised parsing rule
    `in_string` is called. It is supposed to return with a pair made
    of the final value of the thread and of the state, which are then
    fed to the client method for making string tokens,
    `client#mk_string`.

Remains the case of comments, which come in two flavours: multi-line
comments, or _block comments_, and one-line comments, or _line
comments_. As explained above, they have to start with a different
sequence of characters. The semantic actions of the two kinds of
comments are very similar, so it will suffice to explain one of them,
say line comments:

```
| line_comments as lexeme {
    match state#config#line with
      Some opening when opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread             = Thread.make region in
        let thread             = thread#push_string lexeme in
        let thread, state      = in_line thread state lexbuf
        in state#mk_line thread |> mk_markup
    | Some _ | None -> (* Not a comment for this syntax *)
        rollback lexbuf; client#callback state lexbuf }
```

The regular expression `line_comments` gathers the possible openings
for a line comment:

```
let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment
| michelson_line_comment
| jsligo_line_comment
```

This clearly is an _ad hoc_ definition, due to the fact that regular
expressions are not OCaml expressions, at least they are not viewable
this way through the API of `ocamllex`. This is a downside of
metaprogramming with an external tool, as opposed, for example, to
partial evaluation. Lexers written directly in OCaml would of course
avoid this problem, but would likely be less efficient than the ones
generated by `ocamllex` (which could be acceptable in certain
application domains).

So, first we ascertain whether the lexeme is an actual valid start for
a line comment, as specified in the configuration, hence the pattern
matching:

```
    match state#config#line with
      Some opening when opening = lexeme -> ...
    | Some _ | None -> (* Not a comment for this syntax *) ...
```

If a valid line comment opening, the semantic action is similar to one
for strings (see above), with one difference:

```
        let thread, state      = in_line thread state lexbuf
        in `Markup (state#mk_line thread), state
```

compared with

```
      in_string lexeme thread state lexbuf |> client#mk_string
```

In the former case, we need to call the method `mk_line`, which
belongs firmly on the library side (the `State`) because line comments
are not tokens, and tokens are dealt with on the client side (hence
the `client#mk_string`).

Before moving forth with the scanning of comments and strings, let us
have a look at a parsing rule that functions as a wrapper for `scan`:

```
and init client state = parse
  utf8_bom { state#mk_bom lexbuf |> mk_markup          }
| _        { rollback lexbuf; scan client state lexbuf }
```

The first time the lexer is run, the rule `init` is called, and
subsequent runs call `scan` above. This can be readily seen in the
implementation of the trailer function `mk_scan`, which we saw in the
section about [error handling](#error-handling). Let us then show here
the relevant part:

```
let mk_scan (client: 'token Client.t) =
  let internal_client =
    object
      ...
    end
  and first_call = ref true in
  fun state ->
    let scanner =
      if !first_call then (first_call := false; init) else scan
    in scanner internal_client state
```

We see that a hidden reference `first_call` records whether the
returned lambda has been called before or not. The rationale for the
rule `init` is to parse the optional Byte-Order Mark (BOM).

### The `in_block` Parsing Rule

The parsing of block comments is rather involved for several reasons:

  1. Manage different delimiters, depending on the client.

  2. Manage UTF-8 encoded glyphs.

  3. Manage nested block comments.

  4. Manage strings.

The lexer generator `ocamllex` we use is byte-oriented, meaning that
its lexing buffer is seen as an array of bytes. Clearly, this would be
fine for scanning UTF-8 encoded glyphs, as the lexer is not concerned
with the interpretation of the bytes: this is a task of the text
editor. We have nevertheless to take into account UTF-8 because of
error reporting: it may be that a lexing error occurs _after_ the
comment, and, if the location of that error would be reported in bytes
(lines and horizontal offset, for example), it would be seem wrong if
the text editor interprets UTF-8 and displays a glyph as one
"character". This is why the module `Pos` in
`vendors/ligo-utils/simple-utils` can count in bytes or UTF-8
codepoints. But it has to be instructed to do so, and this is why we
must pay attention to UTF-8 in comments.

Let us go through the different cases of `in_block`.

#### Scanning Nested Comments

#### Closing the Current Comment

#### End-of-Line and End-of-File

#### Scanning Text in UTF-8

### The `in_line` Parsing Rule

### The `in_string` Parsing Rule
