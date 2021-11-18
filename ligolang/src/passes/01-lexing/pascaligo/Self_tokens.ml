(* This module implements a filter on the lexical units of PascaLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list, message) result
  end

(* Filters *)

let ok x = Stdlib.Ok x

type message = string Region.reg

type token = Token.t
type lex_unit = token Core.lex_unit

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup    _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Exported *)

let filter = Utils.(tokens_of <@ Style.check)
