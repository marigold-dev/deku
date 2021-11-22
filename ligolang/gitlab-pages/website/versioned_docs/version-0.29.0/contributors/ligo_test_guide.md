# Testing LIGO

Adding to the LIGO test suite is one of the most accessible ways to contribute. It exposes you to the compiler structure and primitives without demanding a deep understanding of OCaml or compiler development.  

Bonus: you'll become more familiar with LIGO in the process!

Tests are written in OCaml, as LIGO doesn't (yet) have a good way to do automated testing. Thankfully the test code is typically less demanding than the features being tested.  

Tests are currently contained in [src/test](https://gitlab.com/ligolang/ligo/tree/dev/src/test), but most are integration tests which rely on test contracts kept in [src/test/contracts](https://gitlab.com/ligolang/ligo/tree/dev/src/test/contracts). If you're new to LIGO, reading these contracts can be a useful introduction to a particular LIGO syntax. In the future we plan 
to have detailed documentation for each syntax, but at the moment we only have a reference manual for [PascaLIGO](https://gitlab.com/ligolang/ligo/blob/dev/src/passes/01-parsing/pascaligo/Doc/pascaligo.md)

## How To Find Good Test Cases

Your first question is probably "If I'm not already experienced, how do I know what to test?". There are a handful of things you can do to systematically find good test cases. All of them will either get you more familiar with the LIGO code base or LIGO itself. 

### Extending Existing Test Cases

The fastest way to improve LIGO's test coverage is to extend existing test cases. Consider the test cases that already exist, and think of things they don't cover or situations they'll fail in. A good deal of inference is required for this, but it requires minimal experience with the existing code. 

### Studying The Parsers For Gaps In Coverage

LIGO is divided into two parts
- the **front end** handles syntax  
- the **backend** optimises and compiles a core language shared between syntaxes

You can find basic test cases for a particular LIGO syntax by studying its parser. You will find the parser under [src/passes/1-parser](https://gitlab.com/ligolang/ligo/tree/dev/src/passes/01-parsing).  

### Two Useful Test Cases Using LIGO

#### Coverage
> whether we have any testing at all for a particular aspect of a syntax

You can find coverage tests by carefully going over the syntax tree for a syntax (probably best read by looking at its `Parser.mly`) and comparing each branch to the test suite. (These tests are plentiful at the time of writing, but they will eventually be filled in reliably as part of writing a new syntax.)

#### Depth
> features are put through a wide variety of complex scenarios to make sure they stand up to real world use 

One of the best ways to find these is to use LIGO for a real project. This will require some time and energy—not just to learn LIGO but to write projects complex enough to stretch the limits of what the language can do. However, it will also get you used to engaging with LIGO from a developers perspective, asking how things could be better or what features are underdeveloped. If your project has practical use, you will also be contributing to the Tezos/LIGO ecosystem while you learn.  

*Note: because LIGO is open source, in order for us to add your work as a test case it needs to be licensed in a way that's compatible with LIGO.*

### Fuzzing (Speculative)

In the future you'll be able to [use fuzzing](https://en.wikipedia.org/wiki/Fuzzing) to generate test cases for LIGO. Fuzzing is often useful for finding 'weird' bugs on code paths that humans normally wouldn't stumble onto. This makes it a useful supplement to human testing.

## Structure of LIGO Tests

LIGO's OCaml-based tests are written in [alcotest](https://github.com/mirage/alcotest/). However, the tests you encounter in [src/test/integration_tests.ml](https://gitlab.com/ligolang/ligo/blob/dev/src/test/integration_tests.ml) are built on top of some abstractions, currently defined in [src/test/test_helpers.ml](https://gitlab.com/ligolang/ligo/blob/dev/src/test/test_helpers.ml). The use of these can be inferred fairly well from looking at existing tests, but let's break a few of them down for analysis.  

### Assignment Test

We'll first analyse a short integration test for assignment.
    
    let assign () : unit result =
      let%bind program = type_file "./contracts/assign.ligo" in
      let make_expect = fun n -> n + 1 in
      expect_eq_n_int program "main" make_expect

#### assign.ligo
    function main (const i : int) : int is
      begin
        i := i + 1 ;
      end with i


What's going on here?  

We have a function which takes no arguments and returns a `unit result`.  
We then define two variables:
- a `program` which is read from disk and fed to the LIGO compiler
- a comparison function `make_expect` which takes an integer and adds one to it

Using `expect_eq_n_int` the `program`'s main function is run and compared to the result of the same input provided to `make_expect`. Notice that the `main` argument given to `expect_eq_n_int` corresponds to the name of the function in `assign.ligo`.

This gives us a taste of what to expect from these integration tests.   

###  Annotation Test

We can see in more complex tests that we're able to pull the values of arbitrary expressions or function calls from LIGO test contracts. Consider:
    
    let annotation () : unit result =
      let%bind program = type_file "./contracts/annotation.ligo" in
      let%bind () =
        expect_eq_evaluate program "lst" (e_list [])
      in
      let%bind () =
        expect_eq_evaluate program "address" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
      in
      let%bind () =
        expect_eq_evaluate program "address_2" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
      in
      ok ()
    
#### annotation.ligo 
    const lst : list(int) = list [] ;

    const address : address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" ;

    const address_2 : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) ;

What's going on is similar to the last program: `expect_eq_evaluate` runs a program and then pulls a particular named value from the final program state.  

For example, once the program stops running the value of `address` is `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`. The *comparison*, however, is made to a constructed expression.  

Remember that we're testing from OCaml, but the program is written and evaluated as LIGO. In order to provide a proper comparison, we convert our expected test values into LIGO expressions and data. Constructors such as `e_list` and `e_address` provide a bridge between LIGO and OCaml. Their definitions can be found in files such as [src/stages/ast_core/combinators.ml](https://gitlab.com/ligolang/ligo/blob/dev/src/stages/ast_core/combinators.ml), or using [Merlin's definition point finder](https://github.com/ocaml/merlin/wiki). These same functions are used during the simplification stage of LIGO compilation, so becoming familiar with them will help prepare you to work on the [front end](big-picture/front-end.md).

## How To Write A Test For LIGO

What if we want to write a test of our own? If the test is in the integration test vein (which it probably is if you're testing new syntax or features), then the process looks something like:
    
1. Write a test contract which uses the new syntax or feature in [src/test/contracts](https://gitlab.com/ligolang/ligo/tree/dev/src/test/contracts).
2. Write an integration test in [src/test/integration_tests.ml](https://gitlab.com/ligolang/ligo/blob/dev/src/test/integration_tests.ml) in the vein of existing tests, make sure you add it to the test runner that is currently located at the bottom of the file.
3. Write the feature, assuming it doesn't already exist. Build the resulting version of LIGO without errors.
4. Run the test suite, see if your test(s) pass. If they do, you're probably done. If not, it's time to go debugging.
