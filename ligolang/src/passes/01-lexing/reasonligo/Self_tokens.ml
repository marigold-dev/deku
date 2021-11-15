(* This module implements a filter on the lexical units of ReasonLIGO
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

(* Virtual token *)

let es6fun = Token.ES6FUN (object 
  method region = Region.ghost
  method attributes = []
  method payload = ""
end)

(* Inserting the ES6FUN virtual token *)

let insert_es6fun_token tokens =
  let open Token in

  (* Unclosed parentheses are used to check if the parentheses are
     balanced *)

  let rec inner result open_parentheses tokens =
    match tokens with
      (* Balancing parentheses *)
    | (RPAR _ as hd) :: rest ->
         inner (hd :: result) (open_parentheses + 1) rest

      (* let foo = (b: (int, int) => int) => ... *)
    | (LPAR _ as hd)::(COLON _ as c)::(Ident _ as i)::(LPAR _ as l)::rest
         when open_parentheses = 1 ->
         List.rev_append (l::i::c::es6fun::hd::result) rest

      (* let a = (x:int) => x *)
    | (LPAR _ as hd)::(ARROW _ as a)::rest
         when open_parentheses = 1 ->
         List.rev_append (a::es6fun::hd::result) rest

    | (DOT _ as dot) :: (UIdent _ as hd) :: rest ->
      inner (hd :: dot :: result) open_parentheses rest

    (* let a : (A|B) => int = (_a:(|A|B)) => 3 *)
    | (UIdent _ as c) :: (VBAR _ as vbar) ::  rest ->
      inner (vbar :: c :: result) open_parentheses rest
    
    | (_ as hd) :: (UIdent _ as c) :: rest ->
      List.rev_append (c :: hd :: result) rest

      (* let foo = (a: int) => (b: int) => a + b *)
    | (_ as hd)::(ARROW _ as a)::rest
         when open_parentheses = 0 ->
         List.rev_append (a::es6fun::hd::result) rest

      (* ((a: int) => a *)
    | (LPAR _ as hd)::(LPAR _ as a)::rest
         when open_parentheses = 1 ->
         List.rev_append (a::es6fun::hd::result) rest

      (* let x : (int => int) *)
    | (LPAR _ as hd)::rest
         when open_parentheses = 0 ->
         List.rev_append (hd::es6fun::result) rest

      (* Balancing parentheses *)
    | (LPAR _ as hd)::rest ->
        inner (hd::result) (open_parentheses - 1) rest

      (* When the arrow '=>' is not part of a function: *)
    | (RBRACKET _ as hd) :: rest
    | (VBAR _ as hd) :: rest ->
        List.rev_append (hd :: result) rest

      (* let foo : int => int = (i: int) => ...  *)
    | (COLON _ as hd)::(Ident _ as i)::(Let _ as l)::rest
         when open_parentheses = 0 ->
         List.rev_append (l::i::hd::es6fun::result) rest

    | (EQ _ as hd)::rest ->
        List.rev_append (hd::es6fun::result) rest

    | hd::rest ->
        inner (hd::result) open_parentheses rest
    | [] ->
        List.rev result
  in inner [] 0 tokens

let insert_es6fun tokens =
  let open Token in
  let rec inner result = function
    (ARROW _ as a)::rest ->
      inner (insert_es6fun_token (a::result)) rest
  | hd::rest ->
      inner (hd::result) rest
  | [] ->
      List.rev result
  in inner [] tokens

let insert_es6fun = function
  Stdlib.Ok tokens -> insert_es6fun tokens |> ok
| Error _ as err -> err

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Exported *)

let filter = Utils.(insert_es6fun <@ tokens_of <@ Style.check)
