<# Documentation of PascaLIGO

## A Walkthrough for the Impatient

PascaLIGO is an imperative language for writing smart contracts on the
Tezos blockchain. As such, it is compiled to Michelson, the native
language of the Tezos blockchain. Its design is inspired by Pascal,
OCaml and Michelson.

  Why Pascal? One quality of Pascal is that its syntax is supported by
many, unabbreviated keywords and compound constructs are opened and
closed by those keywords, making Pascal programs easily readable and
unambiguous, albeit verbose, a desirable property of smart
contracts. In order to reduce the verbosity, PascaLIGO comes in two
flavours: _verbose_ and _terse_. Roughly speaking, this means that
some keywords can be replaced, in some contexts, by symbols. Those
styles are specified on the command line, for a whole contract, and
the default is "verbose". (See below for examples.) Pascal will not be
your only option though: LIGO offers multiple concrete syntaxes, one
of which is based on Pascal, but another one is based on OCaml --- and
more to come.

If you're coming to LIGO from another language, chances are it is an
imperative language like C++ or Java. In an imperative language,
variables can change value over time through repeated assignment; as
opposed to functional languages like OCaml or Haskell where variables
tend to take one value during their lifetime. Formally, code that
changes the value of a variable is considered *mutating* or to have
*side effects*. An imperative language uses side effects in loops and
other control structures to change the contents of the memory.

For example, here is how the integer value associated to the variable
`x` is incremented:

    x := x + 1;

A loop computing the sum of all integers from 1 to 10 would be written
as follows:

    y := 0;
    for x := 1 to 10
      begin
        y := y + x
      end

  In PascaLIGO, expressions and instructions are
distinguished. Expressions are evaluated and yield values, whilst
instructions are evaluated but do not yield values. Instructions are
meant to perform side-effects, like changing the value of a variable,
whereas expressions are purely computational. For instance, an
expression could be used to calculate an arithmetic mean. Instructions
and expressions can be compounded, and instructions can evaluate
expressions as a means to perform side-effects.

  PascaLIGO is _strongly and statically typed_, which means that the
composition of data and functions is contrained so the compiler can
check that no such composition can fail at run-time, e.g., because of
a meaningless expression. PascaLIGO requires that variables are
declared together with their type and an initial value.

  Declarations of values come in two kinds: either constants or
variables. The former are assigned only once at their declaration, and
the latter can be reassigned. The syntax is slightly different for
both. For example, the variables `x` and `y` above could have been
declared as follows:

    var x : nat := 0n;
    var y : nat := 0n;
    for x := 1n to 10n
      begin
        y := y + x
      end

You may have noticed that the literal `0n` has a letter attached to
the number, this is to disambiguate its type from the integer 0: `n`
stands for 'nat' and '0n' is a number of type nat (natural number)
whose value is zero.

It is possible to define constant variables using the `const` keyword.

    const ten : nat = 10n;
    const eleven : nat = ten + 1n;

These variables have fixed value during their lifetime and **in future
versions of LIGO** will raise an error if code attempts to change
them:

    ten := 0n   // Will raise an error in a future version!

Note that the assignment operator for constants is `=`, but `:=` for
mutable variables (that is, annotated by `var` at their
declaration).

  Similarly, function declarations have their parameters and return
value annotated with their types. For instance,

    function sum (const n : nat; const m : nat) : nat is
      begin
        skip
      end with n + m;

declares a function `sum` that takes as argument two constant natural
numbers and returns their sum. The expression whose value is the
result of calling the function is given after the keyword `with`. Note
that to avoid forgetting a single instruction in a block, you need to
use the non-operation `skip`.

  A another familiar example would be

    function factorial (const n : nat) : nat is
      var m : nat := 0n;
      var f : nat := 1n;
      begin
        if n <= 0n then f := 1n
        else
          for m := 1n to n
          begin
            f := f * m
          end
      end with f

<!--   Like Pascal, PascaLIGO offers procedures, as well as functions. The -->
<!-- difference follows the divide between expressions and instructions: -->
<!-- function calls are expressions, procedure calls are instructions. -->

  In order for a function to be a candidate to be an entrypoint to the
contract, it needs to return a specific type: `list (operation) *
store`, where `list (operation)` is the type for lists of operations
(see below, predefined types) and `store` is the type of the internal
storage of the contract.

  PascaLIGO features predefined types, like integers, natural numbers,
mutez (millionth of a tez), strings, maps, lists etc. and constructs
to combine those into structured types. Amongst those constructs are
the _records_, which group and map names (_fields_) to values of
potentially different types. For instance,

    type point is
      record
        x : int;
        y : int
      end

defines a record type `point` with two fields, each made of a name
and a type. Values of record types are made by assigning a value to
each field (in any order). Like so:

    const origin : point =
      record
        x = 0;
        y = 0
      end

  At this point it is perhaps useful to recall that there are actually
two styles, or flavours, of PascaLIGO: one is "verbose" and the other
is "terse". Those styles differ in the manner compound constructs are
delimited. For example, the type `point` above could have been
alternatively defined as follows in the terse style:

    type point is
      record [
        x : int;
        y : int
      ]

and the value as

    const origin : point = record [x = 0; y = 0];

  When updating the fields of a record, PascaLIGO offers some
syntactic support. If you're only updating a small subset of
fields there is a shorthand: the _record patch_, which corresponds
to a functional update in OCaml. For example,

    var p : point := origin;
    patch p with record y = 10 end;

will update only the field `y` of `p`, leaving `x` unchanged.
Of course, this example is not impressive, but imagine that
one has to update a small number of fields in a large record.

  An alternative syntax, in terse style, is

    patch p with record [y = 10];

  Another way to combine types are the _disjunctive types_ (also
called _algebraic data types_), which are a generalisation of
enumerated types, and found in OCaml. They can be interpreted as being
a disjoint partition of value sets, each being disinguished by a
unique tag, or _data constructor_. For example,

    type u = unit
    type t = A | B of u | C of int * string

See OCaml and, as in OCaml, their values can be matched against
patterns:

    match some_value_of_type_t with
      A        -> "A"
    | B (Unit) -> "B"
    | C (_, s) -> s

Note also how type `u` is just another name for the predefined type
`unit`. This is an instance of a _type alias_.

  Another useful data abstraction, native to PascaLIGO, is the _map_,
which relates values of a given type to values of another given
type. For example, the type of maps from addresses to natural number
is written `map (address, nat)`. The address data type comes from Michelson.

  PascaLIGO is inspired by OCaml, where semicolons are separators, but
they can be used as terminators as well. For example

    type store is
      record
        goal     : nat;                (* Separator  *)
        deadline : timestamp;          (* Separator  *)
        backers  : map (address, nat); (* Separator  *)
        funded   : bool
      end

can alternatively be written

    type store is
      record
        goal     : nat;                (* Separator  *)
        deadline : timestamp;          (* Separator  *)
        backers  : map (address, nat); (* Separator  *)
        funded   : bool;               (* Terminator *)
       end

  PascaLIGO has predefined types that cannot be defined by the
contract programmer. Some of those types are specific to the Tezos
blockchain, like `timestamp` and `address` above. Others are the types
`list` and `map` above, but also `set` (sets of comparable
values). The rationale for the latter is not only that they are native
to Michelson, but also that PascaLIGO does not allow the programmer to
define neither recursive types, nor parametric types. In other words,
_user-defined types are monomorphic and non-recursive_. (Monomorphic
means a unique type.) For example, this limitation precludes defining
lists of values, as the type of lists is an inductive data type: a
list is either empty or a pair made of an item (the first item) and
another list (the remaining items). That is why PascaLIGO features a
_native polymorphic list type_, with the condition that all lists of
values must constrain the type of its values to be monomorphic. For
example, we saw above the type `list (operation)`. This reads as the
type constructor `list` being instantiated with the type
`operation`. A type constructor is like a function from types to
types. Note how the syntax reminds us of the analogy of applying data
constructors, or calling functions.

  As an example of list expression, a non-empty list starts with the
keyword `list` and contains its elements separated by semicolons
(definition by extension):

    list 1; 2; 3 end

Accordingly, the empty list is

    list end

but also has a shorthand in the manner of a keyword, because it is
quite common:

    nil

Note that the use of `nil` is acceptable in both verbose and terse
style.

  To push an element in an existing list (an operation called
_consing_ in functional languages), the infix operator to use is
`#`. It takes on the left the element and on the right the list. For
example, the list containing `1`, then `2` and then all items of list
`l` is written:

    1#(2#l)

or, simply (because the `#` operator is right-associative):

    1#2#l

  All user-definable values in PascaLIGO are monomorphic and must be
annotated with their types at declaration (since their type is
given in the left-hand side). Arithmetic or boolean expressions do not
need to be annotated, but empty lists need to be, like so:

    1#(nil : list (int))

But

    var l : list (int) := nil

is also accepted because the type of `nil` here is available in the
left-hand side.

## Comments

PascaLIGO features comments as blocks, that is, spanning one or more
lines, and inline, that is, spanning the end of a line. The syntax of
the former is based on Pascal:

    (* This is
       a block comment *)

The latter is inspired from C++:

    // This is a line comment

## Predefined types and values

### Primitive types

The predefined types of PascaLIGO are lifted from Michelson, so the
programmer knows that they map exactly to their counterpart in the
generated Michelson, which, in turn, help in assessing the gas cost. Some
of those types are specific to the Tezos blockchain, and, when used,
make PascalLIGO a Domain-Specific Language (DSL).

#### Unit

The predefined type `unit` has a unique value `Unit`. It is useful
when no logical value, that is, information, can be deduced from an
expression, typically performing a side effect.

#### Numerical types

There are three kinds of native numerical types in PascaLIGO: `int`,
`nat` and `tez`.

* The first is the type of signed integers, e.g., `-4`, `0` or
`13`. Note that the value zero has a canonical form, `0`, and no
other, for example `00` is invalid. Also, for the sake of convenience,
underscores are allowed in the literals, like `1_000_000`.

* The second numerical type is the type of the natural numbers, e.g.,
`0n` or `13n`. Note that the `nat` literals must be annotated with the
suffix `n`, which distinguishes them from `int` literals. The same
convenient use of underscores as with integer literals is allowed too
and the canonical form of zero is `0n`.

* The last kind of native numerical type is `tez`, which is a unit of
measure of the amounts (fees, accounts). Beware: the literals of the
type `tez` are annotated with the suffix `mutez`, which stands for
millionth of Tez, for instance, `0mutez` or `1200000mutez`. The same
handy use of underscores as in natural literals help in the writing,
like `1_200_000mutez`.

To see how numerical types can be used in expressions see the sections
"Predefined operators" and "Predefined values".

#### Bytes

Bytes have the same type as in Michelson and the same notation for the
literals. The type `bytes` denotes the arrays of hexadecimal numbers
of even length, prefixed by `0x`. For example, `0xA5` or `0xEFFF`. It
is possible to use underscores to make long bytes more readable, like
so: `0x00_A5_EF`. Odd-lengthed or negative bytes are invalid.

#### Booleans

The type of the booleans is `bool`. The only two boolean values are
`True` and `False`. Note the upper case of the first letters, showing
that those are data constructors. (See variant types below.)

#### Strings

The type of the strings is `string`. String values are a contiguous
series of characters enclosed between double quotes, e.g., `"A
string."`. Control characters (including line breaks) are not allowed
unless they are escaped with a preceding backslash, like `"\n"`.

#### Blockchain-specific types

PascaLIGO features predefined types that are specific to the Tezos
blockchain: `operation`, `address`, `key`, `key_hash`, `signature` and
`timestamp`.

#### Options

When the need for an optional value arises, the programmer can use the
predefined type constructor `option`. It applies to a PascaLIGO type
to make it optional, for example, the type of an optional integer is
`option (int)`. All instances of `option` are variant types. Given a
type `foo`, the values of the type `option (foo)` are either `None` or
`Some (v)` where `v` has type `foo`. For example, `Some (4)` is a
value of type `option (int)`.

Options are an instance of a variant type. See section "Variants".

#### Lists

The type constructor `list` is used to create types of lists. For
example, the type of lists of integers is `list (int)`. All instances
of `list` are variant types with two kinds of values. A list is either
empty, denoted by `nil` or `list end`, or non-empty, in which case the
values it contains are written in order separated by semicolons and
between the keywords `list` and `end`. For example, the list
containing the integers `1`, `2` and `3` is written

    list 1; 2; 3 end

 or

    list 1; 2; 3; end

(terminating semicolon). A complete declaration of the constant list
above would be:

    const my_list : list (int) = list 1; 2; 3 end

The _terse style_ of writing PascaLIGO allows the programmer to write

    list [1;2;3]

instead of `list 1; 2; 3 end` (_verbose style_). For readability's
sake, both styles cannot be mixed in the same contract.

To see what operations are available on lists, see the section
"Predefined operators" and "Predefined functions".

#### Sets

The type constructor `set` is used to create ordered sets of
(comparable) values. For example, the type of sets of integers is `set
(int)`. Set values follow the same convention as lists, e.g.,

    set 3; 1; 2 end

(a terminating semicolon is possible) which is the same value as

    set 1; 2; 3 end

because elements of a set are unordered (even though they have to be
comparable). A complete declaration of the constant map above would
be:

    const my_set : set (int) = set 3; 1; 2 end

The _terse style_ of writing PascaLIGO allows the programmer to write

    list [1;2;3]

instead of `list 1; 2; 3 end` (verbose style). For readability's sake,
both styles cannot be mixed in the same contract.

To see how to update a given set, see the section "Instructions/Set
updates".

#### Maps

The type constructor `map` is used to create mappings between
values. A map can be modelled as a set of _bindings_, and each binding
is a pair of values with the convention that the first component of
the pair is the source, or _key_, and the second is the destination,
or _value_. As such, `map` requires a pair of type arguments, for
example, `map (int, string)` is the type of the maps from integers
(the keys) to strings (the values). A constant map containing a
binding from `1` to `"one"` would be declared as follows:

    const my_map : map (int, string) = map 1 -> "one" end

(A terminating semicolon is possible.) Another example is a map that
denotes a permutation of the set of integers `1`, `2` and `3`:

    const perm : map (int, int) = map 1 -> 3; 2 -> 1; 3 -> 2 end

The _terse style_ of writing PascaLIGO allows the programmer to write

    map [1 -> 3; 2 -> 1; 3 -> 2]

instead of `map 1 -> 3; 2 -> 1; 3 -> 2 end` (_verbose style_). For
readability's sake, both styles cannot be mixed in the same contract.

To see how to update a given map, see the section "Instructions/Map
updates".

#### Big maps

Big maps is a type lifted from Michelson, and called `big_map`. As far
as the language itself is concerned, there is no syntactic difference
between maps and big maps. The interpretation of big maps is specified
by the command-line of the LIGO compiler.

### Predefined operators

#### Numeric values

Values of the same given numerical type (integers, naturals, mutez)
can be used in arithmetic expressions. The comparison operators on
numerical types are: `=`, `<`, `<=`, `>`, `>=`, and, notably, the
difference operator is written `=/=`. The arithmetic operators are the
usual ones: `+`, `-` (binary and unary), `/` and `*` are _overloaded_,
which means that they can apply to arguments of different types. For
example, the following expression is valid:

    3 * 4

as well as

    3n * 4n

#### Booleans

Boolean values can be compared with the predefined operators `=` and
`=/=` (inequality). See the section "Instructions/Conditional" to see
the implicit equality in the tests of conditionals. The dedicated
boolean operators are `and` (logical conjunction), `or` (logical
disjunction) and `not` (logical negation).

#### Strings

Strings can be compared for equality and inequality by means of the
predefined operators `=` and `=/=`. They can be concatenated by means
of the operator right-associative predefined operator `^`, like so:

    "This is a prefix" ^ " and " ^ "this is a suffix."

### Lists

Lists can be augmented by adding an item (as their new head) with the
operator `#`, like so:

    (1,"one")#(2,"two")#nil

which is the list containing the pair `(1,"one")` as first item (head)
and `(2,"two")` as the second and last item. This is the same list as

    list [(1,"one"); (2,"two")]

in terse style (see section "Predefined types and values/Lists").

#### Tuples

Given a tuple `t` with _n_ components, the `i`th component is

    t.i

where `t.0` is the first component. For example, given the declaration

    const t : int * string = (4, "four")

the expression `t.1` has the value `"four"`.

#### Records

Record values can be projected, that is, their fields can be selected,
by means of the dot operator: if a record `r` has a field `f`, then
`r.f` has the value associated to the field `f` in `r`. For example,
given the declarations (in verbose style)

    type rec_t = record f : int end
    const r : rec_t = record f = 4 end

then the value of `r.f` is `4`.

### Predefined functions instructions

Beyond a few operators, PascaLIGO features some predefined values and
functions.

#### Tezos-specific

The following are predefined values lifted from Michelson:
`get_force`, `transaction`, `get_contract`, `amount`, `now`, `source`,
and `sender`.

#### Numeric types

It is always safe to cast a natural number (that is, a value of type
`nat`) to an integer. This is done by calling the predefined function
`int`, like so:

    const m : nat = 7n
    const n : int = int (m)

The absolute value of an integer is a natural number, which provides a
way to cast positive integers into natural numbers, like so:

    const m : int = 6
    const n : nat = abs (m)

#### Strings

The call `string_slice (offset, length, string)` to the predefined
function `string_slice` evaluates in the substring of `string`
starting at the offset `offset` (included, first character being at
offset 0) and of length `length`. The result is actually an optional
string: if `offset + length` is greater than the length of `string`,
the result is `None`, otherwise `Some (substring)`. See section
"Options".

<!-- #### Lists -->

<!-- PascaLIGO offers two kinds of iterators on lists. -->

<!-- The first applies a given function to all the items of a given list, -->
<!-- each call returning the predefined value `Unit`. If the function name -->
<!-- is `f` and the list is `l`, this is expressed as -->

<!--     list_iter (l, f); -->

<!-- Note: `list_iter` is a predefined _procedure_. Procedures are -->
<!-- functions that return `Unit` and whose calls are instructions, not -->
<!-- expressions. The same holds for the iterated function `f` here. See -->
<!-- section "Declarations/Procedures". -->

<!-- For an iterator like `list_iter` to be useful, it needs to be able to -->
<!-- perform a side effect, which user-defined procedures and functions -->
<!-- cannot do. Like so: -->

<!--     function iter (const delta : int; const l : list (int)) : int is -->
<!--       var acc : int := 0 -->
<!--       procedure aggregate (const i : int) is -->
<!--         begin -->
<!--           acc := acc + i -->
<!--         end -->
<!--       begin -->
<!--         aggregate (delta);         // Has no effect on acc -->
<!--         list_iter (l, aggregate)   // Has an effect on acc -->
<!--       end with acc -->

<!-- The other predefined iterator on lists is `list_map`. It is useful -->
<!-- when we need to apply a function to all the items of a list and gather -->
<!-- them into another list, in the same order as the original items. (In -->
<!-- mathematical terms, `list_map` builds the list of the images through -->
<!-- the function.) For instance, the function `iter` -->

<!--     function iter (const l : list (int)) : list (int) is -->
<!--       function incr (const i : int) : int is -->
<!--         begin -->
<!--           skip -->
<!--         end with i+1 -->
<!--       begin -->
<!--         skip -->
<!--       end with list_map (l, incr) -->

<!-- will take a list of integers as a parameter and return a list with the -->
<!-- integers all incremented, e.g., `iter (list [1;2;3])` evaluates in -->
<!-- `list [2;3;4]`. -->

####  Sets

Sets have no built-in operators. Instead, PascaLIGO offers predefined
functions to update sets.

  - The function `set_mem` tests for membership in a set. It is the
    same as the boolean expression

        my_set contains my_value

    which is `True` is the set `my_set` contains the value `my_value`,
    otherwise `False`. In functional form:

        set_mem (my_value, my_set)

  - The function `set_add` adds an element to a set:

        set_add (my_value, my_set)

    evaluates into a set that contains all the elements of the set
    `my_set`, plus the element `my_value` (the original set `my_set`
    is unchanged because there are no side effects on the context of a
    function call). If `my_value` was already present in `my_set`,
    then the returned set is the same as `my_set` (mathematical set).

    PascaLIGO also enables the addition of elements by means of a side
    effect. This is when a mutable variable, that is, a variable
    annotated with `var` at its declaration, needs to be modified by
    an instruction. When that variable is a set, for example, in terse
    style:

        var s : set (int) := set [1;4;2]

    PascaLIGO offers the "patch" instruction to add elements to
    `s`. It takes the mutable set `s` and a set constant (that is, a
    set defined by listing all its elements) and adds all the elements
    of the latter to the former, with a side effect. For example, let
    us add the elements `5`, `3` and `4`, all at once, to `s` (in
    terse style):

        patch s with set [5; 3; 4];

    After that instruction, the value of `s` is `set [1;2;3;4;5]`
    --- which is the same as `set [4;2;1;5;3]`, of course.

  - The function `set_remove` builds a set by taking a set and
    removing a given value from it:

        set_remove (my_value, my_set)

    is a set containing all the elements of the set `my_set`, except
    the element `my_value`. In particular, if `my_value` was not in
    `my_set`, the resulting set is identical to `my_set`.

    There is also a special instruction (as opposed to an expression
    like `set_remove (my_value, my_set)`) to remove elements from a
    set:

        remove my_value from set my_set

    In this case, `my_set` must have been declared with the `var`
    annotation of mutable variables.

  - The function `size` returns the cardinal of a set. For instance,
    the expression (in terse style)

        size (set ["a"; "a"; "b"; "c"])

    has value `3`.

  - Complete iteration on sets is performed by loops. See section
    "Loops".

#### Maps

Currently, maps have less support than sets. PascaLIGO offers the
following functions on maps:

  - Adding bindings to a map is only possible if the map is mutable,
    that is, if it was declared with the annotation `var`, like so, in
    terse style:

        var m : map (int, string) := map [2 -> "deux"; 3 -> "three"]

    Then, a destructive update can be performed with a patch, as with
    sets (in terse style), For example, the instruction

        patch m with map [1 -> "one"; 2 -> "two"];

    modifies the value of `m` to be the same as

        map [1 -> "one"; 2 -> "two"; 3 -> "three"]

  - When exactly one binding is added or modified, PascaLIGO offers a
    shorter syntax by reusing the assignement operator like so:

        my_map[my_key] := my_value;

    That instruction addes the binding from `my_key` to `my_value`
    into the map `my_map` (hiding any previous binding for the key
    `my_key`).

  - To check for the existence of a binding in a map for a given key,
    the programmer should simply reuse the `case` instruction. For
    example, to check whether there is a binding for the key `2` in
    the map `m` above, one would write

        case m[2] with
          Some (value) -> ...  // The value is "two", here.
        | None -> ...          // Should only happen if 2 is not a key.
        end

    (See the section "Instructions/Pattern matching".)

    Generally speaking, the type of `m[k]` is `option (value)` if the
    type of `m` is `map (key, value)` and `k` is of type `key`.

  - Removing a binding from a map requires a special instruction, like

        remove sender from map backers

    where `sender` is a key and `backers` is a map. If the key is
    absent in the map, this instruction is a non-operation.

  - Complete iteration on maps is performed by loops. See section
    "Loops".

#### Failures

When an invariant is not satisfied in a contract, it can fail by using
the keyword `fail` followed by a string as a parameter, like so:

    fail "This is an error message."

Note that, were `fail` a function name, we would write instead

    fail ("This is an error message.")

and, indeed, you can do so, but `fail` is a keyword, like `if`, so you
can chose.


## Declarations

There are several kinds of declarations: types, mutable variables,
constants, functions, fields. Depending on the syntactic context, only
some of those declarations will be allowed. Declarations may be
separated by a semicolon. (Because each declaration starts with a
keyword they can be parsed without separators.)


### Types

Type declarations are found only at top-level, that is, outside any
function. They associate a type name to a type expression. The general
syntax is

    type some_type_name is some_type_expression

where `type` and `is` are keywords.

#### Tuples

Tuples are a nameless product of types, as usually found in
mathematics. A particularly useful case is the _pair_, to wit, `(3,
"three")` is a value of a pair type. A corresponding type could be
declared as follows:

    type my_pair is nat * string

The operators `*` is the product on types.

Surprisingly, few programming languages support a notation for tuples,
counter-examples being found mostly amongst functional languages. An
example of a _triple value_ is: `(-3, True, "whatever")`. The only
possible type expression for it is `int * bool * string`.

#### Records

A record is the product of types (called _field types_) indexed by
names (called _field names_). They are called "struct" in C/C++. For
example, in verbose style:

    type store is
      record
        goal     : tez;              
        deadline : timestamp;
        backers  : map (address, nat);
        funded   : bool
      end

A value of that type could be

    record
      goal     = 10mutez;
      deadline = "...";
      backers  = map end;
      funded   = False
    end

#### Variants

The dual type of product types (that is, record and tuple types) are
the sum types, also called here _variant types_. We have seen an
example above in the option type. For example, the type `option (int)`
has two kinds of values: `None` to mean that there is no integer, and
`Some (n)`, where `n` is an integer. This partioning of the values
(`None` vs. `Some`) is what makes it a variant: a value is either of
one shape or the other.

We can imagine that there are more cases too. Consider:

    type suit is Heart | Spade | Club | Diamond

The type `suit` has exactly four values, each with a unique name. Note
that, just as with the `option` type, those names are not variables:
they are the value itself, and, as such, they are called _data
constructors_.

This kind of definition is found in mainstream programming languages
as _enumerated types_. Similarly, we would declare

    type rank is
        Ace | Two | Three | Four | Five | Six | Seven | Eight
      | Nine | Ten | Jack | Queen | King

And a card could be defined as a pair of a suit and a rank, like so:

    type card is suit * rank

Actually, variant types are more general than enumerated types,
whereby a value is associated to each data constructor, and we have
seen that with the `option` type when we write `Some (4)` (the piece
of data here is `4`). For instance, here is an alternative definition
for the cards:

    type card is
      Heart   of rank
    | Spade   of rank
    | Club    of rank
    | Diamond of rank

so `Heart (Ace)` is a value of type `card` now. We recommend the
reader to read about variant types in OCaml to become acquainted with
their possibilities. (They are also called _algebraic data types_.)

See the section "Instructions/Pattern matching".

#### Aliases

Type aliases are simply another name given to an existing type that
has a name. In other words, the type expression in the declaration is
a type name, like

    type trump is card

### Values

Value declarations are of two kinds: mutable variables and constants.

#### Constants

Constant declarations bind the value of an expression to a value name
and that value cannot be changed. Another way to put it is to say that
these are singly assigned variables. The general shape of the
declaration is

    const my_name : my_type = my_expression

Note the `const` keyword and the constant assignment operator `=` (not
`:=`). For example, given the declaration

    const three : nat = 1n + 2n

the following assignment

    three := three + 2n

_will be invalid in a future version of LIGO_. Remember also that
`three = three + 2n` is valid but is a boolean expression whose value
is `False` (the symbol `=` means "is, by definition" when used in a
declaration, and "equals to" in an expression).

#### Mutable variables

Like constant declarations, declarations of mutable variables bind a
name to a value, but that value can be reassignment after the
declaration. The general shape is

    var my_name : my_type := my_expression

Note the keyword `var` and the assignment operator `:=` (not `=`). For
example, given the declaration

    var counter : nat := 1n

the following assignment

    counter := counter + 2n

is valid and changes the value of the mutable variable `counter` to be
`3n`. This is the semantics found in all imperative languages.

IMPORTANT: Mutable variables cannot be declared at top-level, but only
in the scope of function bodies. This is to avoid global side effects
that hide the control flow and makes static analysis extremely
difficult.

### Functions

Function declarations can occur both at top-level and inside
functions, in the tradition of Pascal. For example,

    function incr_list (const l : list (int)) : list (int) is
      function incr_int (const i : int) : int is
        begin
          skip
        end with i+1
      const item : int = 0
      begin
        var temp : list (int) := nil;
        for item in l
          begin
            temp := incr_int (item) # temp
          end;
        var new_l : list (int) := nil;
        for item in temp
          begin
            new_l := item # new_l
          end
      end with new_l

Here, the function `incr_int` is declared inside the declaration of
the function `incr_list`, which take a list of integers and a list
containing the same integers plus one.

The general shape of a function declaration is

    function my_name ( ... (* parameters here *)) : return_type is
      ... // local declarations here
      begin
        ... // instructions here
      end with ... // return expression here

where `function`, `is`, `begin`, `end` and `with` are keywords. For
example,

    function sum (const n : nat; const m : nat) : nat is
      begin
        skip
      end with n + m;

declares a function `sum` that takes as argument two constant natural
numbers and returns their sum. The expression whose value is the
result of calling the function is given after the keyword `with`. Note
that to avoid forgetting a single instruction in a block, you need to
use the non-operation `skip`.

A another familiar example would be

    function factorial (const n : nat) : nat is
      var m : nat := 0n;
      var f : nat := 1n;
      begin
        if n <= 0n then f := 1n
        else
          for m := 1n to n
          begin
            f := f * m
          end
      end with f

IMPORTANT: The semantics of function calls is particular in PascaLIGO:
_the arguments are always copied_, even if the corresponding parameter
is annotated with `var`. So, given the declarations

    function foo (var n : int) : unit is
      begin
        n := n + 1
      end with Unit

and

    var m : int := 5

the call `foo (m)` will leave the value of `m` invariant.

Moreover, _the environment at the call site is also always
copied_. This is perhaps the most puzzling feature of PascaLIGO
functions (in fact, they should be called _quotations_, not functions,
for that very reason). The environment at a point in the program is
the set of variables in scope (that is, variables we can refer
to). The point here is that if it does not matter if those variables
in the environment have been declared `const` or `var`: they will all
be copied, so the body of the called function will deal with
copies. Let us copy an example seen above:

    function iter (const delta : int; const l : list (int)) : int is
      var acc : int := 0
      function aggregate (const i : int) : unit is
        begin
          acc := acc + i // acc is part of the copied environment
        end
      begin
        aggregate (delta);         // Has no effect on acc
      end with acc

Another spin on this topic consists in declaring that functions are
pure for the caller, that is, _the environment is left unchanged by
function calls_. The only way to observe a change is to receive a
value in return.

IMPORTANT: _Functions cannot be recursive in PascaLIGO_, that is why
loops or iterators are needed.

<!-- ### Procedures -->

<!-- WARNING: Procedures are not implemented in the current version of LIGO, they -->
<!-- will appear in a future version with these semantics but cannot currently be -->
<!-- used. -->

<!-- Procedures are a special kind of functions that return `Unit`. They -->
<!-- are declared as follows: -->

<!--     procedure my_name ( ... (* parameters here *)) is -->
<!--       ... // local declarations here -->
<!--       begin -->
<!--         ... // instructions here -->
<!--       end -->

<!-- Since function calls (see section "Functions") leave the environment -->
<!-- invariant, one may wonder what use there is to procedures. As we have -->
<!-- seen in the section about "Lists" and their iterators, the exception -->
<!-- to this rule are predefined iterators, like `list_iter`. They actually -->
<!-- allow the iterated function to perform side effects. Here is the -->
<!-- example again: -->

<!--     function iter (const delta : int; const l : list (int)) : int is -->
<!--       var acc : int := 0 -->
<!--       procedure aggregate (const i : int) is -->
<!--         begin -->
<!--           acc := acc + i -->
<!--         end -->
<!--       begin -->
<!--         aggregate (delta);         // Has no effect on acc -->
<!--         list_iter (l, aggregate)   // Has an effect on acc -->
<!--       end with acc -->

<!-- (For the keen reader, this is because the iterated function is inlined -->
<!-- by the compiler.) -->

## Instructions

In PascaLIGO distinguishes between expressions and instructions. The
former have values, whilst the latter do not. The latter may perform
side effects, whilst the former cannot.

### Assignments

The most obvious kind of instruction is the assignment:

    i := i + 1

### Loops

There are two kinds of loops in PascaLIGO: bounded and
conditioned. The former is often called a _for_ loop, and the latter a
_while_ loop. The most general is the latter, as is well known, has
has the form

    while my_expression
      begin
        ... // instructions here
      end

in verbose style, or

    while my_expression
      block {
        ... // instructions here
      }

in terse style.

The semantics is the usual: the block (body) of the loop is executed
as long as the value of `my_expression` is `True`.

WARNING: The for loop is not implemented in the current version of LIGO. It will
be added with these semantics in future versions, but cannot be used in programs yet.

The syntax for the _for_ loops is more complex because it used to
iterated on collections like sets and maps. Let us start with a simple
case of iteration on integers: A loop computing the sum of all
integers from 1 to 10 would be written as follows:

    y := 0;
    for x := 1 to 10
      begin
        y := y + x
      end

(Note that this is useless in practice, as a closed-form formula
exists for that computation.)

To iterate on a set `s`, we would write, for instance,

    for e in s
      begin
        ... // instructions
      end

where `e` is bound in turn in increasing order to each element of the
set `s`. For example, given the declarations

    const s : set (int) = set 3; 1; 2 end
    var sum : int := 0

the instruction

    for n in s
      begin
        sum := sum + n
      end

has the side effect of storing `6` (`1+2+3`) in `sum`.

To iterate on maps, the syntax is

    for key -> value in m
      begin
        ... // instructions
      end

Here, the variables `key` and `value` will be bound in increasing
order to each key and value of each binding before the block is
executed.

### Conditionals

Conditionals in PascaLIGO are instructions made of a conditional
expression (that is, a boolean expression), and two clauses. The
general shape is

    if my_condition then true_expression else false_expression

We have seen a familiar example above:

    function factorial (const n : nat) : nat is
      var m : nat := 0n;
      var f : nat := 1n;
      begin
        if n <= 0n then f := 1n
        else
          for m := 1n to n
          begin
            f := f * m
          end
      end with f

### Pattern matching

Pattern matching is a generalisation of some constructs found in
mainstream imperative languages, like `switch` in Java, or `case` in
Pascal, those, in turn, being a generalisation of conditionals. They
provide a way to destructure compounded values without the need to
write a cascade of embedded conditionals. The general syntax is

    case my_expression of
      my_pattern_1 -> my_instruction_1
    | my_pattern_2 -> my_instruction_2
      ...
    | my_pattern_n -> my_instruction_n
    end

First, the `case` construct is an instruction, not an expression, and
this can be seen from the fact that we find instructions on the
right-hand sides of the arrows. The informal meaning is this: evaluate
the expression `my_expression`, then "match" the resulting value with
the pattern `my_pattern_1`. If there is a match (we will define this
later), then `my_instruction_1` is executed; otherwise, try matching
`my_pattern_2` etc. In PascaLIGO, the compiler checks that pattern
matchings are complete, so there is no need for a catch-all pattern:
one will have to match.

Let us consider the a function that receives a pair of integers and
returns their sum, except if any of those is zero, in which case it
returns zero:

    function pair_sum (const p : int * int) : int is
      var res : int := 0
      begin
        case p of
          (0,_) -> skip
        | (_,0) -> skip
        | (m,n) -> res := m + n
        end
      end with res

We can see that the last pattern contains the variables, `m` and `n`,
thus matches any pair of values: conditionals can only evaluate
boolean expressions as their test, whereas pattern matching can bind
variables. The binding is evident here because `m` and `n` are used on
the right-hand side of the arrow of the last case: `m + n`. The
underscores `_` in the first two patterns stand for a unique and
unknown variable.

Here is the logical disjunction:

    function logical_or (const p : bool * bool) : bool is
      var res : bool := True
      begin
        case p of
          (False, False) -> res := False
        | _ -> skip
        end
      end with res

The values that are usually destructured are of variant types, for
which there are many cases. Let us revisit the card game above:

    type suit is Heart | Spade | Club | Diamond
    type face is Ace | King | Queen | Jack
    type pip is
      Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    type rank is
      Face of face
    | Pip  of pip
    type card is
      Ordinary of suit * rank
    | Joker

Individual cards can be declared as follows:

    const jack_of_spade : card = Ordinary (Space, Jack)

A function returning the numerical value of a card could be declared
as follows:

    function val_of_face (const f : face) : nat is
      var val : nat := 0n
      begin
        case f of
          Ace   -> val := 14n
        | King  -> val := 13n
        | Queen -> val := 12n
        | Jack  -> val := 11n
        end
      end with val

    function val_of_pip (const p : pip) : nat is
      var val : nat := 0n
      begin
        case p of
          Ten   -> val := 10n
        | Nine  -> val := 9n
        | Eight -> val := 8n
        | Seven -> val := 7n
        | Six   -> val := 6n
        | Five  -> val := 5n
        | Four  -> val := 4n
        | Three -> val := 3n
        | Two   -> val := 2n
        end
      end with val

    function value (const c : card) : nat is
      var val : nat := 0n
      begin
        case c of
          Ordinary (_, Face (f)) -> val := val_of_face (f)
        | Ordinary (_, Pip  (p)) -> val := val_of_pip (p)
        | Joker                  -> val := 15n
        end
      end with val

### Non-operation

PascaLIGO has an explicit keyword for the non-operation: `skip`. Using
`skip` makes it clear that there was no omission.

## Unsupported Functionalities

### Major Functionalities

- The `for` loop is not supported yet.

<!-- - Procedures are not supported yet. -->

- Nested code blocks are not supported yet.

### Minor Functionalities

- The value `None` cannot be assigned to a variable.

- Assignments to nested maps are not currently supported.

- You cannot patch an empty record.

- Map patches are not supported yet.

- Set patches are not supported yet.

- Removal of bindings in nested maps is not supported yet.

- Elements cannot be removed from a set using the `remove` keyword.
