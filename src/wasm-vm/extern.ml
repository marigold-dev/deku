type value_type =
  | I32
  | I64

type func_type = value_type list * value_type option

type t = Func of func_type * (Memory.t -> Value.t list -> Value.t option)

let func typ func = Func (typ, func)

let to_wasm memory t =
  let wrap func memory args =
    let memory = memory () in
    Option.to_list (func memory args) in
  let open Wasm in
  let type_to_wasm_type t =
    Types.NumType
      (match t with
      | I32 -> I32Type
      | I64 -> I64Type) in
  match t with
  | Func ((params, ret), func) ->
    let func_type =
      let params = List.map type_to_wasm_type params in
      let ret = ret |> Option.map type_to_wasm_type |> Option.to_list in
      Types.FuncType (params, ret) in
    Instance.ExternFunc (Func.alloc_host func_type (wrap func memory))
