(* This module implements a filter on the lexical units of JsLIGO
   and produces tokens to be consumed by the parser. *)

[@@@warning "-42"]

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Utils     = Simple_utils.Utils
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list, message) result
  end

(* Utilities *)

let (<@) = Utils.(<@)

let ok x = Stdlib.Ok x

let apply filter = function
  Stdlib.Ok tokens -> filter tokens |> ok
| Error _ as err   -> err

type message = string Region.reg

type token = Token.t

type lex_unit = token Core.lex_unit

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup (Markup.BlockCom {value; region}) -> Token.BlockCom (Token.wrap value region) :: tokens
    | Core.Markup (Markup.LineCom {value; region}) -> Token.LineCom (Token.wrap value region) :: tokens
    | Core.Markup _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Automatic Semicolon Insertion *)

let automatic_semicolon_insertion tokens =
  let open! Token in
  let rec inner result = function
    (Directive _ as t) :: rest ->
    inner (t :: result) rest
  | (LineCom _ as t) :: rest ->
    inner (t :: result) rest
  | (BlockCom _ as t) :: rest ->
    inner (t :: result) rest
  | (_ as semi) :: (LineCom _ as t) :: rest
  | (_ as semi) :: (BlockCom _ as t) :: rest
  | (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest ->
    inner (t:: semi :: result) rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest
  | token :: (Let _ as t) :: rest ->
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    if r#stop#line < r2#start#line  then (
      inner (t :: SEMI (Token.wrap ";" (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop)) :: token :: result) rest 
    )
    else (
      match token with 
        RBRACE _ as t -> 
        inner (t :: SEMI (Token.wrap ";" (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop)) :: token :: result) rest 
      | _ ->
        inner (t :: token :: result) rest
    )
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in
  inner [] tokens

let automatic_semicolon_insertion units =
  apply automatic_semicolon_insertion units

(* Attributes *)

let attribute_regexp = Str.regexp "@\\([a-zA-Z:0-9_]+\\)"

let collect_attributes str =
  let rec inner result str =
    try (
      let r = Str.search_forward attribute_regexp str 0 in
      let s = Str.matched_group 0 str in
      let s = String.sub s 1 (String.length s - 1) in
      let next = (String.sub str (r + String.length s) (String.length str - (r + + String.length s))) in
      inner (s :: result) next
    )
    with
    | Not_found -> result
  in
  inner [] str

let attributes tokens =
  let open! Token in
  let rec inner result = function
    LineCom c :: tl
  | BlockCom c :: tl ->
      let attributes = collect_attributes c#payload in
      let attributes = List.map (fun e ->
        Attr (Token.wrap e c#region)
      ) attributes in
      inner (attributes @ result) tl
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in
  inner [] tokens

let attributes units = apply attributes units

(* Injection of Zero-Width Spaces *)

let inject_zwsp lex_units =
  let open! Token in
  let rec aux acc = function
    [] -> List.rev acc
  | (Core.Token GT _ as gt1) :: (Core.Token GT reg :: _ as units) ->
      aux (Core.Token (ZWSP reg) :: gt1 :: acc) units
  | unit::units -> aux (unit::acc) units
  in aux [] lex_units

let inject_zwsp units = apply inject_zwsp units

(* DEBUG *)

(* Printing lexical units *)

let print_unit = function
  Core.Token t ->
    Printf.printf "%s\n" (Token.to_string ~offsets:true `Point t)
| Core.Markup m ->
    Printf.printf "%s\n" (Markup.to_string ~offsets:true `Point m)
| Core.Directive d ->
    Printf.printf "%s\n" (Directive.to_string ~offsets:true `Point d)

let print_units units =
  apply (fun units -> List.iter print_unit units; units) units

(* Printing tokens *)

let print_token token =
  Printf.printf "%s\n" (Token.to_string ~offsets:true `Point token)

let print_tokens tokens =
  apply (fun tokens -> List.iter print_token tokens; tokens) tokens


(* insert vertical bar for sum type *)

let vertical_bar_insert tokens =
  let open! Token in
  let rec aux acc insert_token = function
    (VBAR _ as hd) :: tl ->
    aux (hd::acc) false tl
  | (EQ _ as hd) :: tl ->
    if insert_token then (
      List.rev_append (hd :: VBAR (Token.wrap "|" Region.ghost) :: acc) tl
    )
    else (
      List.rev_append (hd :: acc) tl
    )
  | (RBRACKET _ as hd) :: tl -> 
    aux (hd::acc) true tl
  | hd :: tl ->
    aux (hd::acc) insert_token tl
  | [] ->
    List.rev acc
  in
  aux [] false tokens

let vertical_bar_insert tokens =
  let open! Token in
  let rec aux acc = function 
    (VBAR _ as hd) :: tl ->
      aux (vertical_bar_insert (hd::acc)) tl
  | hd :: tl -> aux (hd::acc) tl
  | [] -> List.rev acc
  in aux [] tokens

let vertical_bar_insert units = apply vertical_bar_insert units

(* COMPOSING FILTERS (exported) *)

let filter =
  attributes
  <@ automatic_semicolon_insertion
  <@ vertical_bar_insert
  (*  <@ print_tokens*)
  <@ tokens_of
  (*  <@ print_units*)
  <@ inject_zwsp
  <@ Style.check
