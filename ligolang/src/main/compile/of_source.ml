open Helpers

type file_path = string
type module_name = string

type c_unit = Buffer.t * (file_path * module_name) list

(* we should have on for filename with syntax_opt and one in case of no file *)
let extract_meta ~raise syntax file_name : meta =
  let syntax   = syntax_to_variant ~raise (Syntax_name syntax) (Some file_name) in
  {syntax}

let make_meta ~raise syntax file_name_opt : meta =
  let syntax   = syntax_to_variant ~raise (Syntax_name syntax) file_name_opt in
  {syntax}

let make_meta_from_syntax syntax : meta =
  {syntax}

let compile ~raise ~options ~meta (source_filename:string) : c_unit  =
  preprocess_file ~raise ~options ~meta source_filename

let compile_string ~raise ~options ~meta source : c_unit  =
  preprocess_string ~raise ~options ~meta source

let compile_string_without_preproc source : c_unit  =
  let buffer = Buffer.create 0 in
  Buffer.add_string buffer source;
  (buffer, [])

let compile_contract_input ~raise : options:Compiler_options.t -> meta:meta -> string -> string -> c_unit * c_unit =
    fun ~options ~meta parameter storage ->
  Pair.map ~f:(compile_string ~raise ~options ~meta) (parameter,storage)
