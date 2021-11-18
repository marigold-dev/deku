open Simple_utils.Display
module List = Simple_utils.List

let declarations_ppformat ~display_format f (source_file,decls) =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%s declarations:\n" source_file ;
    List.iter ~f: (fun decl -> Format.fprintf f "%s\n" decl) decls

let declarations_jsonformat (source_file,decls) : json =
  let json_decl = List.map ~f:(fun decl -> `String decl) decls in
  `Assoc [ ("source_file", `String source_file) ; ("declarations", `List json_decl) ]

let declarations_format : 'a format = {
  pp = declarations_ppformat;
  to_json = declarations_jsonformat;
}

let changelog_ppformat ~display_format f changelog =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%s" changelog

let changelog_jsonformat changelog : json =
  `String changelog

let changelog_format : 'a format = {
  pp = changelog_ppformat;
  to_json = changelog_jsonformat;
}

let contract_size_ppformat ~display_format f contract_size =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%d bytes" contract_size

let contract_size_jsonformat contract_size : json =
  `Int contract_size

let contract_size_format : 'a format = {
  pp = contract_size_ppformat;
  to_json = contract_size_jsonformat;
}

module Michelson_formatter = struct
  open Tezos_utils.Michelson

  let pp_hex ppf michelson =
    let hex = Proto_alpha_utils.Memory_proto_alpha.to_hex michelson in
    Format.fprintf ppf "%a" Hex.pp hex

  type michelson_format = [
    | `Text
    | `Json
    | `Hex
  ]

  let michelson_ppformat michelson_format ~display_format f a =
    let mich_pp = fun michelson_format ->  match michelson_format with
      | `Text -> pp
      | `Json -> pp_json
      | `Hex -> pp_hex in
    match display_format with
    | Human_readable | Dev -> (
       let m = Format.asprintf "%a\n" (mich_pp michelson_format) a in
       Format.pp_print_string f m
    )

  let michelson_jsonformat michelson_format a : json = match michelson_format with
    | `Text ->
      let code_as_str = Format.asprintf "%a" pp a in
      `Assoc [("text_code" , `String code_as_str)]
    | `Hex -> 
      let code_as_hex = Format.asprintf "%a" pp_hex a in
      `Assoc [("hex_code" , `String code_as_hex)]
    | `Json ->
      (* Ideally , would like to do that :
      Michelson.get_json a *)
      let code_as_str = Format.asprintf "%a" pp_json a in
      `Assoc [("json_code" , `String code_as_str)]

  let michelson_format : michelson_format -> 'a format = fun mf -> {
    pp = michelson_ppformat mf;
    to_json = michelson_jsonformat mf;
  }
end
