(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

(* VENDOR DEPENDENCIES *)

module Region = Simple_utils.Region

(* The functorised interface *)

module type S = LexerLib.API.LEXER

module Make (Token : Token.S) =
  struct
    type token = Token.t
    module Core = LexerLib.Core

    (* ERRORS *)

    type error =
      Unexpected_character of char
    | Non_canonical_zero
    | Invalid_symbol of string
    | Unsupported_nat_syntax
    | Unsupported_mutez_syntax
    | Unsupported_lang_syntax
    | Invalid_natural
    | Unterminated_verbatim
    | Invalid_linemarker_argument

    let sprintf = Printf.sprintf

    let error_to_string = function
      Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Invalid_symbol s ->
        sprintf "Invalid symbol: %S.\n\
                 Hint: Check the LIGO syntax you use." s
    | Invalid_natural ->
        "Invalid natural number."
    | Unsupported_nat_syntax ->
        "Unsupported nat syntax. Please use annotations instead."
    | Unsupported_mutez_syntax ->
        "Unsupported (mu)tez syntax. Please use annotations instead."
    | Unsupported_lang_syntax ->
        "Unsupported code injection syntax."
    | Unterminated_verbatim ->
       "Unterminated verbatim.\n\
        Hint: Close with \"|}\"."
    | Invalid_linemarker_argument ->
       "Unexpected or invalid linemarker argument.\n\
        Hint: The optional argument is either 1 or 2."

    type message = string Region.reg

    exception Error of message

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

    let mk_verbatim (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_verbatim lexeme region
      in Core.Token token, state

    let mk_bytes bytes state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let token = Token.mk_bytes bytes region
      in Core.Token token, state

    let mk_int state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      match Token.mk_int lexeme region with
        Ok token ->
          Core.Token token, state
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_nat state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      match Token.mk_nat lexeme region with
        Ok token ->
          Core.Token token, state
      | Error Token.Non_canonical_zero_nat ->
          fail region Non_canonical_zero
      | Error Token.Invalid_natural ->
          fail region Invalid_natural
      | Error Token.Unsupported_nat_syntax ->
          fail region Unsupported_nat_syntax

    let mk_mutez state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      match Token.mk_mutez lexeme region with
        Ok token ->
          Core.Token token, state
      | Error Token.Non_canonical_zero_tez ->
          fail region Non_canonical_zero
      | Error Token.Unsupported_mutez_syntax ->
          fail region Unsupported_mutez_syntax

    let mk_tez state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      let lexeme = Z.mul (Z.of_int 1_000_000) (Z.of_string lexeme) in
      match Token.mk_mutez (Z.to_string lexeme ^ "mutez") region with
        Ok token ->
          Core.Token token, state
      | Error Token.Non_canonical_zero_tez ->
          fail region Non_canonical_zero
      | Error Token.Unsupported_mutez_syntax ->
          fail region Unsupported_mutez_syntax

    let format_tez s =
      match String.index s '.' with
        index ->
          let len         = String.length s in
          let integral    = Str.first_chars s index
          and fractional  = Str.last_chars s (len-index-1) in
          let num         = Z.of_string (integral ^ fractional)
          and den         = Z.of_string ("1" ^ String.make (len-index-1) '0')
          and million     = Q.of_string "1000000" in
          let mutez       = Q.make num den |> Q.mul million in
          let should_be_1 = Q.den mutez in
          if Z.equal Z.one should_be_1 then Some (Q.num mutez) else None
      | exception Not_found -> assert false

    let mk_tez_dec state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      let lexeme = Str.(global_replace (regexp "_") "" lexeme) in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      match format_tez lexeme with
        None -> assert false
      | Some tz ->
          match Token.mk_mutez (Z.to_string tz ^ "mutez") region with
            Ok token ->
              Core.Token token, state
          | Error Token.Non_canonical_zero_tez ->
              fail region Non_canonical_zero
          | Error Token.Unsupported_mutez_syntax ->
              fail region Unsupported_mutez_syntax

    let mk_ident state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      let token = Token.mk_ident lexeme region
      in Core.Token token, state

    let mk_attr attr state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let token = Token.mk_attr attr region
      in Core.Token token, state

    let mk_uident state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      let token = Token.mk_uident lexeme region
      in Core.Token token, state

    let mk_lang lang state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let start              = region#start#shift_bytes 1 in
      let stop               = region#stop in
      let lang_reg           = Region.make ~start ~stop in
      let lang               = Region.{value=lang; region=lang_reg} in
      match Token.mk_lang lang region with
        Ok token ->
          Core.Token token, state
      | Error Token.Unsupported_lang_syntax ->
          fail region Unsupported_lang_syntax

    let mk_sym state buffer =
      let Core.{region; lexeme; state} = state#sync buffer in
      match Token.mk_sym lexeme region with
        Ok token ->
          Core.Token token, state
      | Error Token.Invalid_symbol s ->
          fail region (Invalid_symbol  s)

    let mk_eof state buffer =
      let Core.{region; state; _} = state#sync buffer in
      let token = Token.mk_eof region
      in Core.Token token, state

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let decimal    = natural '.' natural
let small      = ['a'-'z']
let capital    = ['A'-'Z']
let letter     = small | capital
let ident      = small (letter | '_' | digit)* |
                 '_' (letter | '_' (letter | digit) | digit)+
let uident    = capital (letter | '_' | digit)*
let attr       = letter (letter | '_' | ':' | digit)*
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as b)
let string     = [^'"' '\\' '\n']*  (* For strings of #include *)
let directive  = '#' (blank* as space) (small+ as id) (* For #include *)

(* Symbols *)

let common_sym     =   ";" | "," | "(" | ")"  | "[" | "]"  | "{" | "}"
                     | "=" | ":" | "|" | "." | "_" | "^"
                     | "+" | "-" | "*" | "/"  | "<" | "<=" | ">" | ">="
let pascaligo_sym  = "->" | "=/=" | "#" | ":="
let cameligo_sym   = "->" | "<>" | "::" | "||" | "&&" | "'"
let reasonligo_sym = "!" | "=>" | "!=" | "==" | "++" | "..."
                     | "||" | "&&"
let jsligo_sym     = "++" | "--" | "..." | "?" | "&" | "!" | "~" | "%"
                     | "<<<" | "==" | "!=" | "+=" | "-=" | "*=" | "/="
                     | "%=" | "<<<=" | "&=" | "|="
                     | "^=" | "=>" (* | ">>>" | ">>>=" *)

let symbol =
  common_sym
| pascaligo_sym
| cameligo_sym
| reasonligo_sym
| jsligo_sym

(* RULES *)

(* The scanner [scan] has a parameter [state] that is threaded
   through recursive calls. *)

rule scan state = parse
  ident                  { mk_ident   state lexbuf }
| uident                 { mk_uident  state lexbuf }
| bytes                  { mk_bytes b state lexbuf }
| natural "n"            { mk_nat     state lexbuf }
| natural "mutez"        { mk_mutez   state lexbuf }
| natural ("tz" | "tez") { mk_tez     state lexbuf }
| decimal ("tz" | "tez") { mk_tez_dec state lexbuf }
| natural                { mk_int     state lexbuf }
| symbol                 { mk_sym     state lexbuf }
| eof                    { mk_eof     state lexbuf }
| "[@" (attr as a) "]"   { mk_attr  a state lexbuf }
| "[%" (attr as l)       { mk_lang  l state lexbuf }

| "`" | "{|" as lexeme {
    if lexeme = fst Token.verbatim_delimiters then
      let Core.{region; state; _} = state#sync lexbuf in
      let thread = Core.mk_thread region in
      let verb_end = snd Token.verbatim_delimiters
      in scan_verbatim verb_end thread state lexbuf |> mk_verbatim
    else
      let Core.{region; _} = state#sync lexbuf
      in fail region (Unexpected_character lexeme.[0]) }

| _ as c { let Core.{region; _} = state#sync lexbuf
           in fail region (Unexpected_character c) }

(* Scanning verbatim strings *)

and scan_verbatim verbatim_end thread state = parse
  (* Inclusion of Michelson code *)
  '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ (('1' | '2') as flag))? blank* {
    let Core.{state; region; _} = state#sync lexbuf
    in eol verbatim_end region line file flag thread state lexbuf
  }
| nl as nl { let ()    = Lexing.new_line lexbuf
             and state = state#set_pos (state#pos#new_line nl) in
             scan_verbatim verbatim_end (thread#push_string nl) state lexbuf }
| eof      { fail thread#opening Unterminated_verbatim }
| "`"
| "|}" as lexeme  {
  if verbatim_end = lexeme then
    Core.(thread, (state#sync lexbuf).state)
  else
    let Core.{state; _} = state#sync lexbuf in
    scan_verbatim verbatim_end (thread#push_string lexeme) state lexbuf }
| _ as c   { let Core.{state; _} = state#sync lexbuf in
             scan_verbatim verbatim_end (thread#push_char c) state lexbuf }

and eol verbatim_end region_prefix line file flag thread state = parse
  nl | eof { let _, state =
               Core.linemarker region_prefix ~line ~file ?flag state lexbuf
             in scan_verbatim verbatim_end thread state lexbuf }
| _        { let Core.{region; _} = state#sync lexbuf
             in fail region Invalid_linemarker_argument }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    (* Encoding a function call in exception-raising style (ERS) to
       error-passing style (EPS) *)

    let lift scanner lexbuf =
      try Stdlib.Ok (scanner lexbuf) with
        Error msg -> Stdlib.Error msg

    (* Function [scan] is the main exported function *)

    let client : token Core.client =
      let open Simple_utils.Utils in
      object
        method mk_string = mk_string
        method mk_eof    = lift <@ mk_eof
        method callback  = lift <@ scan
        method support_string_delimiter = support_string_delimiter
      end

    let scan = Core.mk_scan client

  end (* of functor [Make] in HEADER *)
(* END TRAILER *)
}
