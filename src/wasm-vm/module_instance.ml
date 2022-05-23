open Helpers
open Wasm

type t = Instance.module_inst

let main = Utf8.decode "main"
let memory = Utf8.decode "memory"

module Uninit = struct
  type t = (unit -> Instance.module_inst) ref
  let empty : t = ref (fun () -> Errors.raise `Initialization_error)
  let set (t : t) value = t := fun () -> value
  let get t = !t ()
end

let get_memory t =
  match Instance.export t memory with
  | Some (ExternMemory memory) -> memory
  | _ -> Errors.raise `Initialization_error

let syscall_fn custom memory args =
  let point = List.hd args in
  let arg =
    match Value.to_int64 point with
    | Some x -> x
    | None -> Errors.raise `Execution_error in
  custom memory arg;
  Some
    (Values.I64Num.of_num 0 (Values.as_num point) |> Int64.to_int32 |> Value.i32)

let make ~gas ~module_ ~custom =
  let custom =
    let custom = syscall_fn custom in
    Extern.[func ([I64], Some I32) custom] in
  let uninit = Uninit.empty in
  let imports =
    List.map (Extern.to_wasm (fun () -> get_memory (Uninit.get uninit))) custom
  in
  let instance = Eval.init gas module_ imports in
  (* XXX: Is this the better way of doing this? *)
  Uninit.set uninit instance;
  instance

let get_memory t =
  match get_memory t with
  | t -> Ok t
  | exception Errors.Error e -> Error e

let get_entrypoint t =
  match Instance.export t main with
  | Some (ExternFunc func) -> Ok func
  | Some _ -> Error `Execution_error
  | None -> Error `Execution_error
