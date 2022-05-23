open Wasm

module S = Set.Make (struct
  type t = Instance.extern

  (* @TODO this is bad *)
  let compare x y =
    match (x, y) with
    | Instance.ExternFunc a, Instance.ExternFunc b ->
      compare (Func.type_of a) (Func.type_of b)
    | _ -> 0
end)
let extern =
  Instance.ExternFunc
    Types.(
      HostFunc (FuncType ([NumType I64Type], [NumType I32Type]), fun x -> x))

let () =
  Import.register (Utf8.decode "env") (fun name _ ->
      if Utf8.encode name = "syscall" then
        extern
      else
        Errors.raise `Module_validation_error)

type t = Ast.module_
let get_memory t =
  let memory = Utf8.decode "memory" in
  match Instance.export t memory with
  | Some (ExternMemory _) -> ()
  | _ -> Errors.raise `Module_validation_error

let validate_main t s1 ~gas =
  let main = Utf8.decode "main" in
  let validate_entrypoint t =
    match Instance.export t main with
    | Some
        (ExternFunc
          (Func.AstFunc
            ( Types.(
                FuncType ([NumType I32Type], [NumType I64Type; NumType I64Type])),
              _,
              _ ))) ->
      ()
    | Some _ -> Errors.raise `Module_validation_error
    | None -> Errors.raise `Module_validation_error in
  let inst =
    try Wasm.Eval.init gas t (S.to_seq s1 |> List.of_seq) with
    | Wasm.Eval.Link _ -> Errors.raise `Initialization_error in
  validate_entrypoint inst;
  get_memory inst

let predef = S.of_list [extern]

let validate t ~gas =
  let s1 = Import.link t |> S.of_list in
  if S.equal (S.diff s1 predef) S.empty then
    validate_main t s1 ~gas
  else
    Errors.raise `Module_validation_error

let of_string ~gas ~code =
  try
    let module_ =
      match Parse.string_to_module code with
      | { it = Script.Textual module_; at = _ } ->
        Valid.check_module module_;
        let () = validate ~gas module_ in
        module_
      | { it = Script.Quoted _; at = _ }
      | { it = Script.Encoded _; at = _ } ->
        assert false in
    Ok module_
  with
  | Errors.Error err -> Error err
  | Parse.Syntax (_, _)
  | Valid.Invalid (_, _) ->
    (* TODO: Better error reporting *)
    Error `Module_validation_error

let encode t =
  try Ok (Encode.encode t) with
  | Encode.Code (_, string) -> Error string

let decode t =
  try Ok (Decode.decode "" t) with
  | Decode.Code (_, string) -> Error string
