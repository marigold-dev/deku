open Helpers
open Core
module Bytes = Stdlib.Bytes
let main = Wasm.Utf8.decode "main"
let invoke custom ~module_ ~gas ~argument ~storage =
  let t = Module_instance.make ~gas ~module_ ~custom in
  let%ok memory = Module_instance.get_memory t in
  let%ok entrypoint = Module_instance.get_entrypoint t in
  let storage_size = Int64.of_int (Bytes.length storage) in
  let () = Memory.store_bytes memory ~address:0L ~content:storage in
  let () = Memory.store_bytes memory ~address:storage_size ~content:argument in
  Result.try_with (fun () ->
      (* TODO: Encode operations on the return *)
      let[@warning "-8"] Wasm.
                           [
                             Values.Num (I64 address);
                             Values.Num (I64 size);
                             Values.Num (I64 op_offset);
                           ] =
        Wasm.Eval.invoke gas entrypoint
          [Value.i32 (Int64.to_int32_exn storage_size)] in
      let op_size =
        Bytes.get_int32_le
          (Memory.load_bytes memory ~address:op_offset ~size:4)
          0
        |> Int.of_int32_exn in
      let rec go acc ptr to_go =
        if to_go = 0 then
          List.rev acc
        else
          let loaded = Memory.load_bytes memory ~size:4 ~address:ptr in
          let parsed = Bytes.get_int32_le loaded 0 |> Int.of_int32_exn in
          go (parsed :: acc) Int64.(ptr + 5L) (to_go - 1) in
      let ops =
        if op_size = 0 then [] else go [] Int64.(op_offset + 5L) op_size in
      let result =
        (Memory.load_bytes memory ~address ~size:Int.(of_int64_exn size), ops)
      in
      result)
  |> Result.map_error ~f:(fun _ ->
         Printexc.print_backtrace stdout;
         `Execution_error)
