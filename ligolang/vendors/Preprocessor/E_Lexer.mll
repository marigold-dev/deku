(* Auxiliary scanner for boolean expressions of the C# preprocessor *)

{
(* START OF HEADER *)

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

let sprintf = Printf.sprintf

open E_Parser

(* Concrete syntax of tokens. See module [E_Parser]. *)

let string_of_token = function
      True -> "true"
|    False -> "false"
| Ident id -> id
|       OR -> "||"
|      AND -> "&&"
|       EQ -> "=="
|      NEQ -> "!="
|      NOT -> "!"
|     LPAR -> "("
|     RPAR -> ")"
|      EOL -> "EOL"

(* Errors *)

type error = Invalid_character of char

let error_to_string = function
  Invalid_character c ->
    sprintf "Invalid character '%c' (%d)." c (Char.code c)

let format ?(offsets=true) Region.{region; value} ~file =
  let msg   = error_to_string value
  and reg   = region#to_string ~file ~offsets `Byte in
  let value = sprintf "%s:\n%s\n" reg msg
  in Region.{value; region}

exception Error of string Region.reg

let stop value region =
  let error =
    format ~offsets:true ~file:true Region.{region; value}
  in raise (Error error)

let mk_reg buffer =
  let start = Lexing.lexeme_start_p buffer |> Pos.from_byte
  and stop  = Lexing.lexeme_end_p   buffer |> Pos.from_byte
  in Region.make ~start ~stop

let fail error buffer = stop error (mk_reg buffer)

(* END OF HEADER *)
}

(* Regular expressions for literals *)

(* White space *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'

(* Unicode escape sequences *)

let digit = ['0'-'9']
let hexdigit = digit | ['A'-'F' 'a'-'f']
let four_hex = hexdigit hexdigit hexdigit hexdigit
let uni_esc = "\\u" four_hex | "\\U"  four_hex four_hex

(* Identifiers *)

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase | uni_esc
let start = '_' | letter
let alphanum = letter | digit | '_'
let ident = start alphanum*

(* Rules *)

rule scan = parse
  blank+      { scan lexbuf }
| newline     { Lexing.new_line lexbuf; EOL }
| eof         { EOL       }
| "true"      { True      }
| "false"     { False     }
| ident as id { Ident id  }
| '('         { LPAR      }
| ')'         { RPAR      }
| "||"        { OR        }
| "&&"        { AND       }
| "=="        { EQ        }
| "!="        { NEQ       }
| "!"         { NOT       }
| "//"        { inline_com lexbuf }
| _ as c      { fail (Invalid_character c) lexbuf }

and inline_com = parse
  newline { Lexing.new_line lexbuf; EOL }
| eof     { EOL }
| _       { inline_com lexbuf }

{
  (* START OF TRAILER *)
  (* END OF TRAILER *)
}
