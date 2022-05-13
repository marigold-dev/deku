open Wasm

type t = {
  module_ : Ast.module_;
  storage : bytes;
}

let make ~storage ~code =
  try
    let module_ =
      match Parse.string_to_module code with
      | { it = Script.Textual module_; at = _ } ->
        Valid.check_module module_;
        module_
      | _ -> assert false in
    Ok { storage; module_ }
  with
  | Parse.Syntax (_, error)
  | Valid.Invalid (_, error) ->
    (* TODO: Better error reporting *)
    Error error
