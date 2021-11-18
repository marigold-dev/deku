(* Lexer specification for LIGO, to be processed by [ocamllex].

   The underlying design principles are:

     (1) provide precise error messages with hints as how to fix the
         issue, which is achieved by consulting the lexical
         right-context of lexemes;

     (2) be as independent as possible from the LIGO version, so
         upgrades have as little impact as possible on this
         specification: this is achieved by using the most general
         regular expressions to match the lexing buffer and broadly
         distinguish the syntactic categories, and then delegating a
         finer, second analysis to an external module making the
         tokens (hence a functor below);

     (3) support unit testing (lexing of the whole input with debug
         traces).

     A limitation to the independence with respect to the LIGO version
   lies in the errors that the external module building the tokens
   (which may be version-dependent) may have to report. Indeed these
   errors have to be contextualised by the lexer in terms of input
   source regions, so useful error messages can be printed, therefore
   they are part of the signature [Token.S] that parameterises the
   functor generated here. For instance, if, in a future release of
   LIGO, new tokens are added, and the recognition of their lexemes
   entails new errors, the signature [Token.S] will have to be
   augmented and this lexer specification changed. However, in
   practice, it is more likely that instructions or types will be
   added, instead of new kinds of tokens. *)

(* The functorised interface *)

module type S = LexerLib.API.LEXER

module Make (Token : Token.S) : S with type token = Token.t
