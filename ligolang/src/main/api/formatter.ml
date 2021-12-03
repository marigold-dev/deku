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
  open Simple_utils

  let pp_hex ppf michelson =
    let hex = Proto_alpha_utils.Memory_proto_alpha.to_hex michelson in
    Format.fprintf ppf "%a" Hex.pp hex

  type michelson_format = [
    | `Text
    | `Json
    | `Hex
  ]


  type michelson_comments =
    { location : bool;
      source : bool;
      env : bool }

  let comment michelson_comments =
    match michelson_comments with
    | {location; source = _; env = _} ->
      fun loc ->
        if location && not (Simple_utils.Location.is_virtual loc)
        then Some (Format.asprintf "%a" Location.pp loc)
        else None

  let rec yojson_to_json (x : Yojson.Safe.t) : Data_encoding.Json.t =
    match x with
    | `Tuple xs -> `A (List.map ~f:yojson_to_json xs)
    | `Bool b -> `Bool b
    | `Intlit n -> `String n
    | `Null -> `Null
    | `Variant (tag, arg) ->
      `A (`String tag :: List.map ~f:yojson_to_json (Option.to_list arg))
    | `Assoc kvs ->
      `O (List.map ~f:(fun (k, v) -> (k, yojson_to_json v)) kvs)
    | `List vs -> `A (List.map ~f:yojson_to_json vs)
    | `Float f -> `Float f
    | `String s -> `String s
    | `Int n -> `String (string_of_int n)

  let location_encoding : Ast_typed.location Data_encoding.t =
    Data_encoding.(conv
                     (fun loc ->
                        if Location.is_virtual loc
                        then `Null
                        else `O [("location", yojson_to_json (Location.to_human_yojson loc))])
                     (fun _s -> failwith ("internal error: not implemented @ " ^ __LOC__))
                     Data_encoding.json)

  let comment_encoding michelson_comments =
    match michelson_comments with
    | {location; source = _; env = _} ->
      if location
      then Some location_encoding
      else None

  let michelson_ppformat michelson_format michelson_comments ~display_format f a =
    let mich_pp = fun michelson_format michelson_comments ->  match michelson_format with
      | `Text -> pp_comment ~comment:(comment michelson_comments)
      | `Json ->
        pp_json ?comment:(comment_encoding michelson_comments)
      | `Hex -> pp_hex in
    match display_format with
    | Human_readable | Dev -> (
       let m = Format.asprintf "%a\n" (mich_pp michelson_format michelson_comments) a in
       Format.pp_print_string f m
    )

  let michelson_jsonformat michelson_format michelson_comments a : json = match michelson_format with
    | `Text ->
      let code_as_str = Format.asprintf "%a" (pp_comment ~comment:(comment michelson_comments)) a in
      `Assoc [("text_code" , `String code_as_str)]
    | `Hex -> 
      let code_as_hex = Format.asprintf "%a" pp_hex a in
      `Assoc [("hex_code" , `String code_as_hex)]
    | `Json ->
      (* Ideally , would like to do that :
      Michelson.get_json a *)
      let code_as_str = Format.asprintf "%a" (pp_json ?comment:(comment_encoding michelson_comments)) a in
      `Assoc [("json_code" , `String code_as_str)]


  let convert_michelson_comments : [> `All | `Env | `Location | `Source ] list -> michelson_comments =
    let none = {location = false; source = false; env = false} in
    let all = {location = true; source = true; env = true} in
    List.fold_left
      ~init:none
      ~f:(fun config option ->
          let config2 =
            match option with
            | `All -> all
            | `Env -> { none with env = true }
            | `Location -> { none with location = true }
            | `Source -> { none with source = true } in
          { location = config.location || config2.location ;
            source = config.source || config2.source ;
            env = config.env || config2.env })

  let michelson_format : michelson_format -> _ -> 'a format = fun mf michelson_comments -> {
    pp = michelson_ppformat mf (convert_michelson_comments michelson_comments);
    to_json = michelson_jsonformat mf (convert_michelson_comments michelson_comments);
  }
end
