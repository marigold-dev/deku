module M = Memory
open Wasm
open Helpers

type t = {
  contract : Contract.t;
  instance : Instance.module_inst;
}

let main = Utf8.decode "main"
let memory = Utf8.decode "memory"

let make ~contract ~imports gas =
  let instance_ref = ref None in
  let memory () =
    (* Is ok to raise an error here since this is called only from the
       inside of the module after it has been validated. *)
    match Instance.export (Option.get !instance_ref) memory with
    | Some (ExternMemory memory) -> memory
    | _ -> assert false in
  let imports = List.map (Extern.to_wasm memory) imports in
  (* TODO: Missing proper error handling *)
  let instance = Eval.init gas contract.Contract.module_ imports in
  (* XXX: Is this the better way of doing this? *)
  instance_ref := Some instance;
  { contract; instance }

let invoke t gas argument =
  let%ok memory =
    match Instance.export t.instance memory with
    | Some (ExternMemory memory) -> Ok memory
    | Some _ -> Error "Memory exported is not a memory"
    | None -> Error "Should export a memory" in
  let%ok entrypoint =
    (* TODO: Verify entrypoint signature beforehand? *)
    match Instance.export t.instance main with
    | Some (ExternFunc func) -> Ok func
    | Some _ -> Error "Exported entrypoint is not a function"
    | None -> Error "Entrypoint not found, should export a main function." in
  let argument, storage =
    let argument, storage =
      M.blit memory 0L argument;
      (* It is trivial to have an arugment 0 when the argument position is known,
         * but then we can make sure that we can put the argument wherever we want to. *)
      (0L, Int64.of_int @@ Bytes.length argument) in
    M.blit memory storage t.contract.storage;
    (Int64.to_int32 argument, Int64.to_int32 storage) in
  match
    (* TODO: Encode operations on the return *)
    Eval.invoke gas entrypoint [Value.i32 argument; Value.i32 storage]
  with
  | [Values.Num (I32 size)] ->
    Ok
      (Bytes.of_string
         (Memory.load_bytes memory (Int64.of_int32 storage) (Int32.to_int size)))
  | _ -> Error "Wrong return type"
  | (exception Eval.Link (_, error))
  | (exception Eval.Crash (_, error))
  | (exception Eval.Exhaustion (_, error))
  | (exception Eval.Trap (_, error)) ->
    Error error
