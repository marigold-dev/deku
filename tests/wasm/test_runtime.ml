exception Invocation_error

let invoke ?(imports = []) ~storage ~argument code =
  let contract =
    match Wasm_vm.Contract.make ~storage ~code with
    | Ok contract -> contract
    | Error msg -> Alcotest.fail msg in
  let gas = ref 10000 in
  let runtime = Wasm_vm.Runtime.make ~contract ~imports gas in
  match Wasm_vm.Runtime.invoke runtime gas argument with
  | Ok storage -> storage
  | Error msg ->
    Printf.eprintf "%s\n" msg;
    raise Invocation_error

let test_simple_invocation () =
  let code =
    {|
    (module
      (memory (export "memory") 1)
      (func (export "main") (param i32) (param i32) (result i32)
        local.get 1
        local.get 0
        i64.load
        local.get 1
        i64.load
        i64.add
        i64.store
        i32.const 8))
  |}
  in
  let storage = Hexdump.of_string {|
      01 00 00 00 00 00 00 00
    |} in
  let argument = Hexdump.of_string {|
      2A 00 00 00 00 00 00 00
    |} in
  let storage = invoke ~storage ~argument code in
  Alcotest.(check Hexdump.hex)
    "Same"
    (Hexdump.of_string "2B 00 00 00 00 00 00 00")
    storage

let test_extern_bindings () =
  let code =
    {|
    (module
      (import "env" "opaque" (func $opaque (result i64)))
      (memory (export "memory") 1)
      (func (export "main") (param i32) (param i32) (result i32)
        local.get 1
        i64.const 24
        call $opaque
        i64.add
        i64.store
        i32.const 8))
    |}
  in
  let storage = Bytes.empty in
  let argument = Bytes.empty in
  let called = ref false in
  let opaque_fn _ _ =
    called := true;
    Some (Wasm_vm.Value.i64 13L) in
  let imports = Wasm_vm.[Extern.func ([], Some I64) opaque_fn] in
  let storage = invoke ~imports ~storage ~argument code in
  Alcotest.(check Hexdump.hex)
    "Same"
    (Hexdump.of_string "25 00 00 00 00 00 00 00")
    storage;
  Alcotest.(check bool) "Called" true !called

let test_extern_manipulating_memory () =
  let code =
    {|
    (module
      (import "env" "mock_hash" (func $mock_hash (param i32) (param i32) (param i32)))
      (memory (export "memory") 1)
      (func (export "main") (param $argument i32) (param $storage i32) (result i32)
        local.get $argument
        i32.const 8
        local.get $storage
        call $mock_hash
        i32.const 32))
    |}
  in
  let storage = Bytes.empty in
  let argument = Hexdump.of_string "AB CD EF 01 02 03 04 05" in
  let mock_hash memory args =
    let open Wasm_vm in
    let payload, size, target =
      match args with
      | [payload; size; target] -> (payload, size, target)
      | _ -> assert false in
    let payload, size, target =
      match
        (Value.to_int32 payload, Value.to_int32 size, Value.to_int32 target)
      with
      | Some payload, Some size, Some target ->
        (Int64.of_int32 payload, Int32.to_int size, Int64.of_int32 target)
      | _ -> assert false in
    let payload = Memory.sub memory payload size in
    Alcotest.(check Hexdump.hex) "Same buffer" argument payload;
    let hash =
      Hexdump.of_string
        "8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92" in
    Memory.blit memory target hash;
    None in
  let imports = Wasm_vm.Extern.[func ([I32; I32; I32], None) mock_hash] in
  let storage = invoke ~imports ~storage ~argument code in
  Alcotest.(check int) "Same size" 32 (Bytes.length storage);
  Alcotest.(check Hexdump.hex)
    "Same"
    (Hexdump.of_string
       "8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92")
    storage

let test_memory_export () =
  let code =
    {|
    (module
      (func (export "main") (param i32) (param i32) (result i32)
        i32.const 0))
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
  let code = {|
    (module
      (memory (export "memory") 1))
    |} in
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
      test_case "External memory manipulation" `Quick
        test_extern_manipulating_memory;
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
