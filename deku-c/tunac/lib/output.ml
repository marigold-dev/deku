type t =
  { module_ : string
  ; constants : (int * Values.t) array
  ; entrypoints : Path.t option
  }
[@@deriving yojson]

let make module_ constants entrypoints =
  let open Wasm.Script in
  let open Wasm.Source in
  try
    let m = Wasm.Parse.string_to_module module_ in
    match m.it with
    | Textual m ->
      Wasm.Valid.check_module m;
      Array.sort (fun (x, _) (x2, _) -> Int.compare x x2) constants;
      Ok { module_; constants; entrypoints }
    | Encoded _ | Quoted _ -> Error `Invalid_module
  with Wasm.Parse.Syntax (at, msg) | Wasm.Valid.Invalid (at, msg) ->
    Format.eprintf "Module validation error at %d:%d - %d:%d: %s" at.left.line
      at.left.column at.right.line at.right.column msg;
    Error `Module_validation_error
