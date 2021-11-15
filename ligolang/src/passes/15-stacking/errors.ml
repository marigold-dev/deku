open Simple_utils.Display

type stacking_error = [
  | `Stacking_corner_case of string * string
  | `Stacking_contract_entrypoint of string
  | `Stacking_bad_iterator of Mini_c.constant'
  | `Stacking_not_comparable_pair_struct
  | `Stacking_could_not_tokenize_michelson of string
  | `Stacking_could_not_parse_michelson of string
  | `Stacking_untranspilable of int Michelson.t * int Michelson.t
  | `Spilling_unsupported_primitive of Stage_common.Types.constant' * Environment.Protocols.t
]

let stage = "stacking"
let unstacking_stage = "unstacking_stage"
let corner_case_msg () = 
  "Sorry, we don't have a proper error message for this error. Please report \
   this use case so we can improve on this."

let unsupported_primitive c p = `Spilling_unsupported_primitive (c,p)
let corner_case ~loc  message = `Stacking_corner_case (loc,message)
let contract_entrypoint_must_be_literal ~loc = `Stacking_contract_entrypoint loc
let bad_iterator cst = `Stacking_bad_iterator cst
let not_comparable_pair_struct = `Stacking_not_comparable_pair_struct
let unrecognized_data errs = `Stacking_unparsing_unrecognized_data errs
let untranspilable m_type m_data =
  let open Tezos_micheline.Micheline in
  let m_type = root (strip_locations m_type) in
  let m_data = root (strip_locations m_data) in
  `Stacking_untranspilable (m_type, m_data)
let bad_constant_arity c = `Stacking_bad_constant_arity c
let could_not_tokenize_michelson c = `Stacking_could_not_tokenize_michelson c
let could_not_parse_michelson c = `Stacking_could_not_parse_michelson c

let error_ppformat : display_format:string display_format ->
  Format.formatter -> stacking_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Spilling_unsupported_primitive (c,p) ->
      Format.fprintf f "@[<hv>unsupported primitive %a in protocol %s@]" Stage_common.PP.constant' c (Environment.Protocols.variant_to_string p)
    | `Stacking_corner_case (loc,msg) ->
      let s = Format.asprintf "Stacking corner case at %s : %s.\n%s"
        loc msg (corner_case_msg ()) in
      Format.pp_print_string f s ;
    | `Stacking_contract_entrypoint loc ->
      let s = Format.asprintf "contract entrypoint must be given as a literal string: %s"
        loc in
      Format.pp_print_string f s ;
    | `Stacking_bad_iterator cst ->
       let s = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
       Format.pp_print_string f s ;
    | `Stacking_not_comparable_pair_struct ->
      let s = "Invalid comparable value. When using a tuple with more than 2 components, structure the tuple like this: \"(a, (b, c))\". " in
      Format.pp_print_string f s;
    | `Stacking_could_not_tokenize_michelson code ->
      Format.fprintf f "Could not tokenize raw Michelson: %s" code
    | `Stacking_could_not_parse_michelson code ->
      Format.fprintf f "Could not parse raw Michelson: %s" code
    | `Stacking_untranspilable (ty, value) ->
      Format.fprintf f "Could not untranspile Michelson value: %a %a"
        Michelson.pp ty
        Michelson.pp value
  )

let error_jsonformat : stacking_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Spilling_unsupported_primitive (_,_) ->
    let content = `Assoc [
      ("message", `String "unsupported primitive" );
      ]
    in
    json_error ~stage ~content
  | `Stacking_corner_case (loc,msg) ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String msg); ] in
    json_error ~stage ~content
  | `Stacking_contract_entrypoint loc ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String "contract entrypoint must be given as literal string"); ] in
    json_error ~stage ~content
  | `Stacking_bad_iterator cst ->
    let s = Format.asprintf "%a" Mini_c.PP.constant cst in
    let content = `Assoc [
       ("message", `String "bad iterator");
       ("iterator", `String s); ]
    in
    json_error ~stage ~content
  | `Stacking_not_comparable_pair_struct ->
    let content = `Assoc [
       ("message", `String "pair does not have a comparable structure");
       ("hint", `String "use (a,(b,c)) instead of (a,b,c)"); ]
    in
    json_error ~stage ~content
  | `Stacking_could_not_tokenize_michelson code ->
    let content =
      `Assoc [("message", `String "Could not tokenize raw Michelson");
              ("code", `String code)] in
    json_error ~stage ~content
  | `Stacking_could_not_parse_michelson code ->
    let content =
      `Assoc [("message", `String "Could not parse raw Michelson");
              ("code", `String code)] in
    json_error ~stage ~content
  | `Stacking_untranspilable (ty, value) ->
    let content =
      `Assoc [("message", `String "Could not untranspile Michelson value");
              ("type", `String (Format.asprintf "%a" Michelson.pp ty));
              ("value", `String (Format.asprintf "%a" Michelson.pp value))] in
    json_error ~stage ~content
