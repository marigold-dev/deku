open Helpers

let main = Wasm.Utf8.decode "main"

let invoke custom ~module_ ~gas ~argument ~storage =
  let t = Module_instance.make ~gas ~module_ ~custom in
  let%ok memory = Module_instance.get_memory t in
  let%ok entrypoint = Module_instance.get_entrypoint t in
  let storage_size = Bytes.length storage |> Int64.of_int in
  let () = Memory.store_bytes memory ~address:0L ~content:storage in
  let () = Memory.store_bytes memory ~address:storage_size ~content:argument in
  match
    (* TODO: Encode operations on the return *)
    Wasm.Eval.invoke gas entrypoint [Value.i32 (Int64.to_int32 storage_size)]
  with
  | Wasm.[Values.Num (I64 address); Values.Num (I64 size)] ->
    Ok (Memory.load_bytes memory ~address ~size:(Int64.to_int size))
  | _ -> Error `Execution_error
  | exception Errors.Error err -> Error err
  | (exception Wasm.Eval.Crash (_, ee))
  | (exception Wasm.Eval.Exhaustion (_, ee))
  | (exception Wasm.Eval.Trap (_, ee)) ->
    Format.printf "%s\n" ee;
    Error `Execution_error
