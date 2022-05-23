exception Invocation_error
open Helpers
let invoke ?(custom = fun _ _ -> ()) ~storage ~argument code =
  match
    let%ok contract = Wasm_vm.Module.of_string ~gas:(ref max_int) ~code in
    let gas = ref 10000 in
    Wasm_vm.Runtime.invoke custom ~module_:contract ~gas ~argument ~storage
  with
  | Ok storage -> storage
  | Error _ -> raise Invocation_error

let i32 t =
  let b = Bytes.make 4 '0' in
  let () = Bytes.set_int32_le b 0 t in
  b

let i64 t =
  let b = Bytes.make 8 '0' in
  let () = Bytes.set_int64_le b 0 t in
  b

let test_simple_invocation () =
  let code =
    {|
          (module
            (import "env" "syscall" (func $syscall (param i64) (result i32)))
            (memory (export "memory") 1)
            (func (export "main")  (param i32) (result i64 i64)
              i32.const 0
              i32.const 0
              i64.load
              local.get 0
              i64.load
              i64.add
              i64.store
              (i64.const 0)
              (i64.const 8)
              ))
        |}
  in
  let storage = i64 1L in
  let argument = i64 42L in
  let storage = invoke ~storage ~argument code in
  Format.printf "%LX\n" 1L;
  Alcotest.(check Hexdump.hex)
    "Same"
    (Hexdump.of_string "2B 00 00 00 00 00 00 00")
    storage

let test_extern_bindings () =
  let code =
    {|
          (module
            (import "env" "syscall" (func $syscall (param i64) (result i32)))
            (memory (export "memory") 1)
            (func (export "main")  (param i32) (result i64 i64)
              i32.const 0
              i64.const 0
              i64.store
              i32.const 9
              i64.const 42
              i64.store
              i64.const 0
              call $syscall
              (i64.extend_i32_s)
              (i64.const 8)
              ))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  let called = ref false in
  let custom mem arg =
    called := true;
    let buf = Wasm_vm.Memory.load_bytes mem ~address:arg ~size:8 in
    let num = Bytes.get_int64_le buf 0 in
    match num with
    | 0L ->
      let i64m = i64 in
      let i64 =
        Wasm_vm.Memory.load_bytes mem ~address:Int64.(add arg 9L) ~size:8 in
      let i64 = Bytes.get_int64_le i64 0 in
      let i64 = Int64.add i64 1L in
      Wasm_vm.Memory.store_bytes mem ~address:0L ~content:(i64m i64)
    | _ -> assert false in
  let storage = invoke ~custom ~storage ~argument code in
  Alcotest.(check Hexdump.hex)
    "Same"
    (Hexdump.of_string "2B 00 00 00 00 00 00 00")
    storage;
  Alcotest.(check bool) "Called" true !called

let test_memory_export () =
  let code =
    {|
          (module
            (func (export "main") (param i32) (result i64 i64)
              i64.const 0 i64.const 0))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_memory_name () =
  let code =
    {|
          (module
            (memory (export "foo") 1)
            (func (export "main") (param i32) (param i32) (result i32)
              i32.const 0))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_memory_type () =
  let code =
    {|
          (module
            (func (export "memory"))
            (func (export "main") (param i32) (param i32) (result i32)
              i32.const 0))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_main_export () =
  let code =
    {|
          (module
            (memory (export "memory") 1))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_main_name () =
  let code =
    {|
          (module
            (memory (export "memory") 1)
            (func (export "foo") (param i32) (param i32) (result i32)
              i32.const 0))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_main_type () =
  let code =
    {|
          (module
            (memory (export "memory") 1)
            (global (export "main") (mut i32) (i32.const 0)))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_main_signature () =
  let code =
    {|
          (module
            (memory (export "memory") 1)
            (func (export "main") (result i32)
              i32.const 0))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_division_by_zero () =
  let code =
    {|
          (module
            (memory (export "memory") 1)
            (func (export "main") (param i32) (param i32) (result i32)
              i32.const 1
              i32.const 0
              i32.div_s))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_trap_unreachable () =
  let code =
    {|
          (module
            (memory (export "memory") 1)
            (func (export "main") (param i32) (param i32) (result i32)
              unreachable))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test_incorrect_return_type () =
  let code =
    {|
          (module
            (memory (export "memory") 1)
            (func (export "main") (param i32) (param i32) (result i64)
              i64.const 0))
          |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore (invoke ~storage ~argument code))

let test =
  let open Alcotest in
  ( "Runtime",
    [
      test_case "Simple invocation" `Quick test_simple_invocation;
      test_case "External calls" `Quick test_extern_bindings;
      test_case "Export memory" `Quick test_memory_export;
      test_case "Export memory name" `Quick test_memory_name;
      test_case "Export memory type" `Quick test_memory_type;
      test_case "Export main" `Quick test_main_export;
      test_case "Export main name" `Quick test_main_name;
      test_case "Export main type" `Quick test_main_type;
      test_case "Main signature" `Quick test_main_signature;
      test_case "Main return type" `Quick test_incorrect_return_type;
      test_case "Division by zero" `Quick test_division_by_zero;
      test_case "Trap unreachable" `Quick test_trap_unreachable;
    ] )
