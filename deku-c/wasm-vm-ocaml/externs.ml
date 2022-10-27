let push = Wasm.Utf8.decode "push"
let main = Wasm.Utf8.decode "main"
let call_callback_unit = Wasm.Utf8.decode "call_callback_unit"
let call_callback = Wasm.Utf8.decode "call_callback"

let extract_func name inst =
  match Wasm.Instance.export name inst with
  | Some Wasm.(Instance.ExternFunc (Func.AstFunc (_, _, _) as func)) -> Ok func
  | _ -> Error `Bad_contract
