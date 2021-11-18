open Simple_utils
open Stage_common.Helpers
open Stage_common.Types

let binder_attributes_of_strings (ss : string list) : binder_attributes =
  if List.mem ~equal:String.equal ss "var" then
      var_attribute
  else if List.mem ~equal:String.equal ss "const" then
    const_attribute
  else
    empty_attribute

let strings_of_binder_attributes
      (lang : [`CameLIGO | `ReasonLIGO | `PascaLIGO | `JsLIGO ])
      (attributes : binder_attributes) : string list =
  let pureligo {const_or_var} =
    match const_or_var with
                | Some `Var -> ["var"]
                | _ -> [] in
  let impureligo {const_or_var} =
    match const_or_var with
                | Some `Var -> ["var"]
                | Some `Const -> ["const"]
                | _ -> [] in
  match lang with
  | `CameLIGO | `ReasonLIGO -> pureligo attributes
  | `PascaLIGO | `JsLIGO -> impureligo attributes
