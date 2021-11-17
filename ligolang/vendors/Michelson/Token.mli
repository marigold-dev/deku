(* This signature defines the lexical tokens for Michelson.

   _Tokens_ are the abstract units which are used by the parser to
   build the abstract syntax tree (AST), in other words, the stream of
   tokens is the minimal model of the input program, carrying
   implicitly all its structure in a linear encoding, and nothing
   else, in particular, comments and whitespace are absent.

     A _lexeme_ is a specific character string (concrete
   representation) denoting a token (abstract representation). Tokens
   can be thought of as sets, and lexemes as elements of those sets --
   there is often an infinite number of lexemes, but a small number of
   tokens. (Think of identifiers as lexemes and one token.)

     The tokens are qualified here as being "lexical" because the
   parser generator Menhir expects to define them, in which context
   they are called "parsing tokens", and they are made to match each
   other. (This is an idiosyncratic terminology.)

     The type of the lexical tokens is the variant [t], also
   aliased to [token].
*)


(* Vendor dependencies *)

module Region = Simple_utils.Region
module Markup = LexerLib.Markup

(* TOKENS *)

type lexeme = string

(* Annotations *)

val max_annot_length : int (* Current maximum: 255-character long *)

type annotation =
  Tannot of string Region.reg  (* Type  annotations *)
| Vannot of string Region.reg  (* Value annotations *)
| Fannot of string Region.reg  (* Field annotations *)

val annot_to_lexeme : annotation -> lexeme

val annot_to_string :
  offsets:bool -> [`Byte | `Point] -> annotation -> string

(* Keywords *)

type keyword =
  K_parameter of Region.t  (* parameter *)
| K_storage   of Region.t  (* storage   *)
| K_code      of Region.t  (* code      *)

val keyword_to_lexeme : keyword -> lexeme

val keyword_to_string :
  offsets:bool -> [`Byte | `Point] -> keyword -> string

(* Data *)

type data =
  D_Elt   of Region.t  (* "Elt"   *)
| D_False of Region.t  (* "False" *)
| D_Left  of Region.t  (* "Left"  *)
| D_None  of Region.t  (* "None"  *)
| D_Pair  of Region.t  (* "Pair"  *)
| D_Right of Region.t  (* "Right" *)
| D_Some  of Region.t  (* "Some"  *)
| D_True  of Region.t  (* "True"  *)
| D_Unit  of Region.t  (* "Unit"  *)

val data_to_lexeme : data -> lexeme

val data_to_string :
  offsets:bool -> [`Byte | `Point] -> data -> string

(* Instructions *)

type instruction =
  ABS              of Region.t
| ADD              of Region.t
| ADDRESS          of Region.t
| AMOUNT           of Region.t
| AND              of Region.t
| APPLY            of Region.t
| BALANCE          of Region.t
| BLAKE2B          of Region.t
| CHAIN_ID         of Region.t
| CAST             of Region.t
| CHECK_SIGNATURE  of Region.t
| COMPARE          of Region.t
| CONCAT           of Region.t
| CONS             of Region.t
| CONTRACT         of Region.t
| CREATE_CONTRACT  of Region.t
| DIG              of Region.t
| DIP              of Region.t
| DROP             of Region.t
| DUG              of Region.t
| DUP              of Region.t
| EDIV             of Region.t
| EMPTY_BIG_MAP    of Region.t
| EMPTY_MAP        of Region.t
| EMPTY_SET        of Region.t
| EQ               of Region.t
| EXEC             of Region.t
| FAILWITH         of Region.t
| GE               of Region.t
| GET              of Region.t
| GT               of Region.t
| HASH_KEY         of Region.t
| IF               of Region.t
| IF_CONS          of Region.t
| IF_LEFT          of Region.t
| IF_RIGHT         of Region.t
| IMPLICIT_ACCOUNT of Region.t
| INT              of Region.t
| ISNAT            of Region.t
| ITER             of Region.t
| LAMBDA           of Region.t
| LE               of Region.t
| LEFT             of Region.t
| LOOP             of Region.t
| LOOP_LEFT        of Region.t
| LSL              of Region.t
| LSR              of Region.t
| LT               of Region.t
| MAP              of Region.t
| MEM              of Region.t
| MUL              of Region.t
| NEG              of Region.t
| NEQ              of Region.t
| NIL              of Region.t
| NONE             of Region.t
| NOT              of Region.t
| NOW              of Region.t
| OR               of Region.t
| PACK             of Region.t
| PUSH             of Region.t
| RENAME           of Region.t
| RIGHT            of Region.t
| SELF             of Region.t
| SENDER           of Region.t
| SET_DELEGATE     of Region.t
| SHA256           of Region.t
| SHA512           of Region.t
| SIZE             of Region.t
| SLICE            of Region.t
| SOME             of Region.t
| SOURCE           of Region.t
| SUB              of Region.t
| SWAP             of Region.t
| TRANSFER_TOKENS  of Region.t
| UNIT             of Region.t
| UNPACK           of Region.t
| UPDATE           of Region.t
| XOR              of Region.t

val instr_to_lexeme : instruction -> lexeme

val instr_to_string :
  offsets:bool -> [`Byte | `Point] -> instruction -> string

(* Macros

   Macros are syntactical notations that are not predefined in the
   semantics of Michelson. Instead, they provide a convenient and
   compact way to express common patterns of instructions.

     Some macros, here called "constant", have a constant
   name. Others, called "non-constant" or "extensible" (our
   terminology) have variable names. In the type [macro], the latter
   are associated to the shortest form of their name, so, for
   instance, the macro "PPAIPAIR" and "PAIR" have the same data
   constructor PAIR, except with different values. *)

type macro =
  (* Constant macros *)

  ASSERT        of Region.t
| ASSERT_CMPEQ  of Region.t
| ASSERT_CMPGE  of Region.t
| ASSERT_CMPGT  of Region.t
| ASSERT_CMPLE  of Region.t
| ASSERT_CMPLT  of Region.t
| ASSERT_CMPNEQ of Region.t
| ASSERT_EQ     of Region.t
| ASSERT_GE     of Region.t
| ASSERT_GT     of Region.t
| ASSERT_LE     of Region.t
| ASSERT_LEFT   of Region.t
| ASSERT_LT     of Region.t
| ASSERT_NEQ    of Region.t
| ASSERT_NONE   of Region.t
| ASSERT_RIGHT  of Region.t
| ASSERT_SOME   of Region.t
| CMPEQ         of Region.t
| CMPGE         of Region.t
| CMPGT         of Region.t
| CMPLE         of Region.t
| CMPLT         of Region.t
| CMPNEQ        of Region.t
| FAIL          of Region.t
| IFCMPEQ       of Region.t
| IFCMPGE       of Region.t
| IFCMPGT       of Region.t
| IFCMPLE       of Region.t
| IFCMPLT       of Region.t
| IFCMPNEQ      of Region.t
| IFEQ          of Region.t
| IFGE          of Region.t
| IFGT          of Region.t
| IFLE          of Region.t
| IFLT          of Region.t
| IFNEQ         of Region.t
| IF_NONE       of Region.t
| IF_SOME       of Region.t

  (* Non-constant macros *)

| PAIR     of Pair.tree Region.reg
| UNPAIR   of Pair.tree Region.reg
| CADR     of Pair.path Region.reg
| SET_CADR of Pair.path Region.reg
| MAP_CADR of Pair.path Region.reg

val macro_to_lexeme : macro -> lexeme

val macro_to_string :
  offsets:bool -> [`Byte | `Point] -> macro -> string

(* Types *)

type m_type =
  T_address   of Region.t  (* address   *)
| T_big_map   of Region.t  (* big_map   *)
| T_bool      of Region.t  (* bool      *)
| T_bytes     of Region.t  (* bytes     *)
| T_chain_id  of Region.t  (* chain_id  *)
| T_contract  of Region.t  (* contract  *)
| T_int       of Region.t  (* int       *)
| T_key       of Region.t  (* key       *)
| T_key_hash  of Region.t  (* key_hash  *)
| T_lambda    of Region.t  (* lambda    *)
| T_list      of Region.t  (* list      *)
| T_map       of Region.t  (* map       *)
| T_mutez     of Region.t  (* mutez     *)
| T_nat       of Region.t  (* nat       *)
| T_operation of Region.t  (* operation *)
| T_option    of Region.t  (* option    *)
| T_or        of Region.t  (* or        *)
| T_pair      of Region.t  (* pair      *)
| T_set       of Region.t  (* set       *)
| T_signature of Region.t  (* signature *)
| T_string    of Region.t  (* string    *)
| T_timestamp of Region.t  (* timestamp *)
| T_unit      of Region.t  (* unit      *)

val type_to_lexeme : m_type -> lexeme

val type_to_string :
  offsets:bool -> [`Byte | `Point] -> m_type -> string


(* TOKENS *)

(*
type t =
  String  of lexeme Region.reg
| Bytes   of (lexeme * Hex.t) Region.reg
| Int     of (lexeme * Z.t) Region.reg
| Keyword of keyword
| Data    of data
| Instr   of instruction
| Macro   of macro
| Type    of m_type
| Annot   of annotation
| SEMI    of Region.t
| LPAREN  of Region.t
| RPAREN  of Region.t
| LBRACE  of Region.t
| RBRACE  of Region.t
| EOF     of Region.t

type token = t
 *)

type token
type t = token

(* Projections

   The difference between extracting the lexeme and a string from a
   token is that the latter is the textual representation of the OCaml
   value denoting the token (its abstract syntax), rather than its
   lexeme (concrete syntax). *)

val to_lexeme : token -> lexeme
val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
val to_region : token -> Region.t

(* Injections *)

type int_err = Non_canonical_zero

type ident_err =
  Valid_prefix       of Pair.index * Pair.tree
| Invalid_tree       of Pair.index * char * Pair.tree
| Truncated_encoding of Pair.index * Pair.child * Pair.tree
| Missing_break      of int
| Invalid_identifier

type annot_err = Annotation_length of int

val mk_string : lexeme -> Region.t -> token
val mk_bytes  : lexeme -> Region.t -> token
val mk_int    : lexeme -> Region.t -> (token,   int_err) result
val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
val mk_annot  : lexeme -> Region.t -> (token, annot_err) result
val mk_sym    : lexeme -> Region.t -> token
val eof       : Region.t -> token

(* Predicates *)

val is_string : token -> bool
val is_bytes  : token -> bool
val is_int    : token -> bool
val is_ident  : token -> bool
val is_annot  : token -> bool
val is_sym    : token -> bool
val is_eof    : token -> bool

val support_string_delimiter : char -> bool
