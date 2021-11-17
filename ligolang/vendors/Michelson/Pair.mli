(* This module offers support for checking and transforming lexemes
   for the extensible Michelson macros PAIR, UNPAIR and CADR. *)

(* DIGITS AND CODES *)

(* Encodings of non-empty binary trees with PAIR and UNPAIR. *)

type digit = P | A | I    (* Note: The terminator R is not included. *)
type code  = digit list   (* Note: May become empty (recursively).   *)

(* Injection and projection for digits *)

val char_to_digit : char -> digit (* Note: For the lexer. *)
val digit_to_char : digit -> char (* Note: For the pretty-printer of tokens. *)

(* Injection and projection for codes *)

val lift : string -> code  (* Note: For the lexer. *)
val drop : code -> string  (* Note: For the pretty-printer of tokens. *)

(* NON-EMPTY BINARY TREES *)

(* The type for trees enforces the asymmetric roles of pair
   constructors A and I: the former is a left leaf, whilst the latter
   is a right leaf. Moreover, leaves carry a status indicating whether
   they are specified in a source encoding (see type [code] above) or
   if they where reconstructed to complete the tree (to help the
   programmer fix an error with the encoding). In the former case, the
   constant constructor [Real] of type [status] is used, otherwise
   [Fake]. Note that trees cannot be empty. *)

type status = Fake | Real
type left   = [`Left  of status | `Pair of left * right]
and  right  = [`Right of status | `Pair of left * right]
and  tree   = [`Pair of left * right]

type t = tree

(* Injection and projection of trees, that is, decoding and
   encoding. The former can raise two exceptions corresponding to
   ill-formed codes, see below. As for encoding, keep in mind that the
   resulting code does _not_ include the terminator R. *)

type index = int (* Note: Intended to be a natural number. *)
type child = [`Left | `Right]

(* Decoding errors.

   Here we distinguish two kinds of errors (the lexer refines one kind
   into two, for more precision). Firstly, the code may contain a
   valid prefix, in other words, it appears to be made of a correct
   code, with additional pair constructors. For example,
   PAIAAAAAAR. Secondly, the code may be incorrect, that is, a
   constructor is found to be invalid at a given position, like an `I'
   instead of an `A'. For example, PIAR.

     These errors are captured by values of the type [decode_err],
   which is used in the type of the return value of the decoding
   function, [decode].

     The first kind of error above is specified by means of the
   variant [Valid_prefix], whilst the second is by
   [Invalid_tree]. Both constructors carry an index and a tree. The
   index is the offset in the code of the digit that is either
   extraneous or unexpected, i.e., the wrong kind. The tree is the
   correct tree so far, or the incorrect tree completed with fake
   leaves to show where the structure must be fixed.

     The constructor [Invalid_tree] carries an additional value of
   type [child], which is used by the handler to suggest a fix to the
   Michelson programmer, as that value specifies whether the error
   occurred while decoding a left or a right subtree.
*)

type decode_err =
  Valid_prefix of index * tree
| Invalid_tree of index * tree * child

val decode : code -> (t, decode_err) Stdlib.result
val encode : t -> code

(* Conversions and I/Os

   The call [draw channel tree] displays in ASCII art the tree [tree]
   on the output channel [channel].

   The function [to_string] produces the same textual representation
   as [draw], but produces a string instead of performing a
   side-effect.
*)

val draw : out_channel -> t -> unit
val to_string : t -> string

(* PATHS IN BINARY TREES *)

(* The processing of CADR extensible macros requires support from this
   module in the form of paths in binary trees. *)

type path = child list

val lift_path : string -> path
val drop_path : path -> string
