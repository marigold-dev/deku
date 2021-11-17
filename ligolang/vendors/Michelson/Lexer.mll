(* Lexer specification for Michelson, to be processed by [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

(* VENDOR DEPENDENCIES *)

module Region = Simple_utils.Region

(* TOKENS *)

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. Consequently, generic functions to
   construct tokens are provided. Note predicate [is_eof], which
   caracterises the virtual token for end-of-file, because it requires
   special handling. *)

type lexeme = string

module type TOKEN =
  sig
    type token
    type t = token

    (* Errors *)

    type int_err = Non_canonical_zero

    type annot_err = Annotation_length of int

    type ident_err =
      Valid_prefix       of Pair.index * Pair.tree
    | Invalid_tree       of Pair.index * char * Pair.tree
    | Truncated_encoding of Pair.index * Pair.child * Pair.tree
    | Missing_break      of int
    | Invalid_identifier

    (* Injections *)

    val mk_string : lexeme -> Region.t -> token
    val mk_bytes  : lexeme -> Region.t -> token
    val mk_int    : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
    val mk_annot  : lexeme -> Region.t -> (token, annot_err) result
    val mk_sym    : lexeme -> Region.t -> token
    val eof       : Region.t -> token

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
    val to_region : token -> Region.t

    (* Predicates *)

    val is_string : token -> bool
    val is_bytes  : token -> bool
    val is_int    : token -> bool
    val is_ident  : token -> bool
    val is_annot  : token -> bool
    val is_sym    : token -> bool
    val is_eof    : token -> bool

    val support_string_delimiter : char -> bool
  end

(* The functorised interface *)

module type S = LexerLib.API.LEXER

module Make (Token : TOKEN) =
  struct
    module Token = Token
    type token = Token.t
    module Core = LexerLib.Core

    (* ERRORS *)

    type error =
      Unexpected_character of char
    | Annotation_length of int
    | Invalid_identifier
    | Non_canonical_zero
    | Valid_prefix of Pair.tree
    | Invalid_tree of char * Pair.tree
    | Truncated_encoding of Pair.child * Pair.tree
    | Missing_break

    let sprintf = Printf.sprintf

    let error_to_string = function
      Unexpected_character c ->
        sprintf "Unexpected character '%c'." c
    | Annotation_length max ->
        sprintf "Annotation length exceeds the built-in limit %d." max
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Invalid_identifier ->    (* TODO: Shortest edit distance? *)
        "Invalid identifier."
    | Invalid_tree (char, tree) ->
        let opposite = if char = 'I' then 'A' else 'I' in
        sprintf "\
          Wrong pair constructor '%c'.\n\
          Hint: Try inserting to its left '%c', or remove it.\n\
          Anyhow, follow the following BNF grammar:\n\
          <pair macro> ::= P <left> <right> R\n\
          %s    <left> ::= A | P <left> <right>\n\
          %s   <right> ::= I | P <left> <right>.%s"
          char opposite "  " "  "
          (sprintf "\nLean your head to the left and consider \
                    the tree completed with Xs:\n%s"
                   (Pair.to_string tree))

    | Truncated_encoding (child, tree) ->
        let expected =
          match child with
            `Left -> 'A'
          | `Right -> 'I' in
        sprintf "\
          Unexpected end of encoding.\n\
          Hint: Try inserting to its left '%c' or 'P'.\n\
          Anyhow, follow the following BNF grammar:\n\
          <pair macro> ::= P <left> <right> R\n\
          %s    <left> ::= A | P <left> <right>\n\
          %s   <right> ::= I | P <left> <right>.%s"
          expected "  " "  "
          (sprintf "\nLean your head to the left and consider \
                    the tree completed with Xs:\n%s"
                   (Pair.to_string tree))

    | Valid_prefix tree ->
        sprintf "\
          Extraneous pair constructor.\n\
          Hint: Try removing it.\n\
          Anyhow, follow the following BNF grammar:\n\
          <pair macro> ::= P <left> <right> R\n\
          %s    <left> ::= A | P <left> <right>\n\
          %s   <right> ::= I | P <left> <right>.%s"
          "  " "  "
          (sprintf "\nLean your head to the left and consider \
                    the tree so far:\n%s" (Pair.to_string tree))

    | Missing_break ->
        "Missing break.\n\
         Hint: Insert some space."

    type message = string Region.reg

    exception Error of message (* NOT EXPORTED *)

    (* Encoding a function call in exception-raising style (ERS) to
       error-passing style (EPS) *)

    let lift scanner lexbuf =
      try Stdlib.Ok (scanner lexbuf) with
        Error msg -> Stdlib.Error msg

    (* Decoding a function call in EPS to ERS *)
(*
    let drop scanner lexbuf =
      match scanner lexbuf with
        Stdlib.Ok state -> state
      | Stdlib.Error msg -> raise (Error msg)
 *)
    (* Raising an error *)

    let fail region error =
      let msg = error_to_string error in
      raise (Error Region.{value=msg;region})

    let support_string_delimiter = Token.support_string_delimiter

    (* TOKENS *)

    (* Making tokens *)

    let mk_string (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_string lexeme region
      in Core.Token token, state

    let mk_bytes bytes state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let token = Token.mk_bytes bytes region
      in Core.Token token, state

    let mk_int state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      match Token.mk_int lexeme region with
        Ok token -> Core.Token token, state
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_ident state buffer =
      let mk_region index start =
        let start = start#shift_bytes index in
        let stop  = start#shift_bytes 1
        in Region.make ~start ~stop
      and start = state#pos in
      let Core.{region; lexeme; state} = state#sync buffer in
      match Token.mk_ident lexeme region with
        Ok token -> Core.Token token, state
      | Error Token.Valid_prefix (index, tree) ->
          let region = mk_region index start in
          fail region (Valid_prefix tree)
      | Error Token.Invalid_tree (index, char, tree) ->
          let region = mk_region index start in
          fail region (Invalid_tree (char, tree))
      | Error Token.Truncated_encoding (index, child, tree) ->
          let region = mk_region index start in
          fail region (Truncated_encoding (child, tree))
      | Error Token.Invalid_identifier ->
          fail region Invalid_identifier
      | Error Token.Missing_break index ->
          let start  = start#shift_bytes index in
          let region = Region.make ~start ~stop:start
          in fail region Missing_break

    let mk_annot state buffer =
      let Core.{region; lexeme; state} = state#sync buffer
      in match Token.mk_annot lexeme region with
           Ok token -> Core.Token token, state
         | Error Token.Annotation_length max ->
             fail region (Annotation_length max)

    let mk_sym state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      let token = Token.mk_sym lexeme region
      in Core.Token token, state

    let mk_eof state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let token = Token.eof region
      in Core.Token token, state

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions

   For annotations, see documentation at
   http://tezos.gitlab.io/master/whitedoc/michelson.html#syntax
*)

let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let small      = ['a'-'z']
let capital    = ['A'-'Z']
let integer    = '-'? natural
let letter     = small | capital
let ident      = letter (letter | '_' | digit)*
let hexa_digit = digit | ['A'-'F']
let byte       = hexa_digit hexa_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as seq)
let symbol     = ';' | '(' | ')' | '{' | '}'
let annotation =
  (':'|'@'|'%')
  ('@'|'%'|"%%"| ('_' | letter) ('_' | letter | digit | '.')*)?

(* #include files *)

let string = [^'"' '\\' '\n']*  (* For strings of #include *)

(* RULES *)

(* The scanner [scan] has a parameter [state] that is thread through
   recursive calls. *)

rule scan state = parse
  ident      { mk_ident     state lexbuf }
| bytes      { mk_bytes seq state lexbuf }
| integer    { mk_int       state lexbuf }
| annotation { mk_annot     state lexbuf }
| symbol     { mk_sym       state lexbuf }
| eof        { mk_eof       state lexbuf }
| _ as c     { let Core.{region; _} = state#sync lexbuf
               in fail region (Unexpected_character c) }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let client =
  let open Simple_utils.Utils in
  object
    method mk_string = mk_string
    method mk_eof    = lift <@ mk_eof
    method callback  = lift <@ scan
    method support_string_delimiter = support_string_delimiter
  end

let scan = Core.mk_scan client

(* Style checking *)
(*
let check_right_context config token next_token lexbuf =
  let pos    = (config#to_region token)#stop in
  let region = Region.make ~start:pos ~stop:pos in
  let next lexbuf =
    match next_token lexbuf with
      None -> ()
    | Some (markup, next) ->
        let open Token in
        match markup with
          [] ->
            if   is_int token || is_string token || is_bytes token
            then if   is_sym next || is_eof next
                 then ()
                 else fail region Missing_break
            else ()
        | _::_ -> ()
  in lift next lexbuf
     *)
end (* of functor [Make] in HEADER *)

(* END TRAILER *)
}
