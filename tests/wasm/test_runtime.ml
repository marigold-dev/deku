open Helpers
open Test_helpers
let test_simple_invocation () =
  let code =
    {|
          (module
            (import "env" "syscall" (func $syscall (param i64) (result i32)))
            (memory (export "memory") 1)
            (func (export "main")  (param i32) (result i64 i64 i64)
              i32.const 0
              i32.const 0
              i64.load
              local.get 0
              i64.load
              i64.add
              i64.store
              (i64.const 0)
              (i64.const 8)
              (i64.const 8)
              ))
        |}
  in
  let storage = i64 1L in
  let argument = i64 42L in
  let storage, _ = invoke ~storage ~argument code in
  let storage = Bytes.get_int64_le storage 0 in
  Alcotest.(check int64) "Same" 43L storage

let test_ticket_own () =
  let open Core_deku in
  let code =
    {|
              (module
                (import "env" "syscall" (func $syscall (param i64) (result i32)))
                (memory (export "memory") 1)
                (func (export "main")  (param i32) (result i64 i64 i64)
                  i32.const 21
                  i32.const 5
                  i32.store
                  i32.const 22
                  i32.const 0
                  i32.store
                  i64.const 21 
                  call $syscall
                  i64.extend_i32_s
                  (i64.const 20)
                  (i64.const 45)
                  ))
            |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket in
  let argument = handle |> Ticket_handle.to_bytes in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.unsafe_deposit_ticket table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let module M =
  (val Contract_context.make ~source:addr ~sender:addr ~table
         ~contracts_table:(fun _ -> None)
         ~self:(make_contract_address "test")
         ~tickets:[handle])
  in
  let imit_hanlde, _ =
    let table = Ticket_table.add_to_temporary table [handle] in
    let tt =
      Ticket_table.own table
        (Address.of_contract_hash (make_contract_address "test"))
        handle in
    tt |> Result.get_ok in
  let module Ctx = Wasm_vm.Ffi.Make (M) in
  let storage =
    invoke ~custom:Ctx.custom ~storage ~argument code
    |> fst
    |> Ticket_handle.of_bytes
    |> Option.get in
  Alcotest.(check Testables.ticket_handle) "Same" imit_hanlde storage

let test_ticket_join () =
  let open Core_deku in
  let code =
    {|
                  (module
                    (import "env" "syscall" (func $syscall (param i64) (result i32)))
                    (memory (export "memory") 1)
                    (func (export "main")  (param i32) (result i64 i64 i64)
                      i32.const 41
                      i32.const 6
                      i32.store
                      i32.const 46
                      i32.const 0
                      i32.store
                      i32.const 51
                      i32.const 20
                      i32.store
                      i64.const 41 
                      call $syscall
                      i64.extend_i32_s
                      (i64.const 20)
                      (i64.const 99)
                      ))
                |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket in
  let contract_addr = Address.of_contract_hash @@ make_contract_address "test" in
  let handle2 = Ticket_handle.make contract_addr ticket in
  let argument =
    Bytes.concat Bytes.empty
      [handle |> Ticket_handle.to_bytes; Ticket_handle.to_bytes handle2] in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.unsafe_deposit_ticket table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let table =
    Ticket_table.unsafe_deposit_ticket table ~ticket ~destination:contract_addr
      ~amount:(Amount.of_int 10) in
  let module M =
  (val Contract_context.make ~source:addr ~sender:addr ~table
         ~contracts_table:(fun _ -> None)
         ~self:(make_contract_address "test")
         ~tickets:[handle])
  in
  let imit_hanlde, _ =
    let tt =
      Ticket_table.join_tickets table ~source:contract_addr
        ~senders:(addr, contract_addr) ~handles:(handle, handle2) in
    tt |> Result.get_ok in
  let module Ctx = Wasm_vm.Ffi.Make (M) in
  let storage =
    invoke ~custom:Ctx.custom ~storage ~argument code
    |> fst
    |> Ticket_handle.of_bytes
    |> Option.get in
  Alcotest.(check string)
    "Same"
    (Ticket_handle.to_string imit_hanlde)
    (Ticket_handle.to_string storage)
let test_ticket_split () =
  let open Core_deku in
  let code =
    {|
                      (module
                        (import "env" "syscall" (func $syscall (param i64) (result i32)))
                        (memory (export "memory") 1)
                        (func (export "main")  (param i32) (result i64 i64 i64)
                          i32.const 21
                          i32.const 4
                          i32.store
                          i32.const 26
                          i32.const 0
                          i32.store
                          i32.const 31
                          i64.const 5
                          i64.store
                          i32.const 40
                          i64.const 5
                          i64.store
                          i64.const 21
                          call $syscall
                          i64.extend_i32_s
                          (i64.const 41)
                          (i64.const 99)
                          ))
                    |}
  in

  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket in
  let argument = handle |> Ticket_handle.to_bytes in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.unsafe_deposit_ticket table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let module M =
  (val Contract_context.make ~source:addr ~sender:addr ~table
         ~contracts_table:(fun _ -> None)
         ~self:(make_contract_address "test")
         ~tickets:[handle])
  in
  let imit_hanlde, _ =
    let tt =
      Ticket_table.split_ticket table ~sender:addr ~ticket_handle:handle
        ~amounts:(Amount.of_int 5, Amount.of_int 5) in
    tt |> Result.get_ok in
  let module Ctx = Wasm_vm.Ffi.Make (M) in
  let storage = invoke ~custom:Ctx.custom ~storage ~argument code |> fst in
  let first, second =
    ( Bytes.sub storage 0 20 |> Ticket_handle.of_bytes |> Option.get,
      Bytes.sub storage 20 20 |> Ticket_handle.of_bytes |> Option.get ) in
  let storage = (first, second) in
  let imit_hanlde = (fst imit_hanlde, snd imit_hanlde) in
  Alcotest.(check (pair Testables.ticket_handle Testables.ticket_handle))
    "Same" imit_hanlde storage
let test_ticket_send_twice () =
  let open Core_deku in
  let code =
    {|
                  (module
                    (import "env" "syscall" (func $syscall (param i64) (result i32)))
                    (memory (export "memory") 1)
                    (func (export "main")  (param i32) (result i64 i64 i64)
                      i32.const 67
                      i32.const 8
                      i32.store
                      i32.const 72
                      i32.const 0
                      i32.store
                      i32.const 77
                      i32.const 8
                      i32.store
                      i32.const 82
                      i64.const 10
                      i64.store
                      i32.const 91
                      i32.const 28
                      i32.store
                      i64.const 67
                      call $syscall
                      i64.extend_i32_s
                      (i64.const 0)
                      i32.const 72
                      i32.const 1
                      i32.store 
                      i32.const 77
                      i32.const 1 
                      i32.store
                      (i64.const 72)
                      ))
                |}
  in
  let code2 =
    {|
                  (module
                    (import "env" "syscall" (func $syscall (param i64) (result i32)))
                    (memory (export "memory") 1)
                    (func (export "main")  (param i32) (result i64 i64 i64)
                      i32.const 21
                      i32.const 5
                      i32.store
                      i32.const 26
                      i32.const 0
                      i32.store
                      i64.const 21 
                      call $syscall
                      i64.extend_i32_s
                      (i64.const 20)
                      (i64.const 99)
                      ))
                |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket in
  let contract_address1 = make_contract_address "test" in
  let contract_address2 = make_contract_address "test2" in
  let table = Ticket_table.empty in
  let argument =
    Bytes.concat Bytes.empty
      [
        i64 20L;
        Ticket_handle.to_bytes handle;
        Address.to_string (Address.of_contract_hash contract_address2)
        |> Bytes.of_string
        (* 67 *);
      ] in

  let table =
    Ticket_table.unsafe_deposit_ticket table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let module M =
  (val Contract_context.make ~source:addr ~sender:addr ~table
         ~contracts_table:(fun _ -> None)
         ~self:contract_address1 ~tickets:[handle])
  in
  let module Ctx = Wasm_vm.Ffi.Make (M) in
  let _, ops = invoke ~custom:Ctx.custom ~storage ~argument code in
  let x = M.get_ops ops |> Result.get_ok |> List.hd in
  Alcotest.(check Testables.contract_operation)
    "Same"
    (Contract_context.Contract_operation.Invoke
       {
         ticket;
         destination = Address.of_contract_hash contract_address2;
         param =
           Ticket_handle.to_bytes
             (Ticket_handle.make
                (Address.of_contract_hash contract_address1)
                ticket);
       })
    x;
  let new_table = M.get_table () |> Ticket_table.validate in
  let ticket, param =
    match x with
    | Contract_context.Contract_operation.Invoke
        { destination = _; param; ticket } ->
      (ticket, param)
    | _ -> failwith "wrong op" in
  let module M =
  (val Contract_context.make
         ~sender:(Address.of_contract_hash contract_address1)
         ~table:new_table ~self:contract_address2
         ~tickets:
           [
             Ticket_handle.make
               (Address.of_contract_hash contract_address1)
               ticket;
           ]
         ~source:addr
         ~contracts_table:(fun _ -> None))
  in
  let module Ctx = Wasm_vm.Ffi.Make (M) in
  let storage =
    invoke ~custom:Ctx.custom ~storage ~argument:param code2
    |> fst
    |> Ticket_handle.of_bytes
    |> Option.get in

  let imit_hanlde, _ =
    let table = Ticket_table.add_to_temporary table [handle] in
    let tt =
      Ticket_table.own table (Address.of_contract_hash contract_address2) handle
    in
    tt |> Result.get_ok in
  Alcotest.(check Testables.ticket_handle) "Same" imit_hanlde storage

let test_ticket_send_implicit () =
  let open Core_deku in
  let code =
    {|
                      (module
                        (import "env" "syscall" (func $syscall (param i64) (result i32)))
                        (memory (export "memory") 1)
                        (func (export "main")  (param i32) (result i64 i64 i64)
                          i32.const 67
                          i32.const 8
                          i32.store
                          i32.const 72
                          i32.const -1
                          i32.store
                          i32.const 77
                          i32.const 8
                          i32.store
                          i32.const 82
                          i64.const 10
                          i64.store
                          i32.const 91
                          i32.const 28
                          i32.store
                          i64.const 67
                          call $syscall
                          i64.extend_i32_s
                          (i64.const 0)
                          i32.const 72
                          i32.const 1
                          i32.store 
                          i32.const 77
                          i32.const 1 
                          i32.store
                          (i64.const 72)
                          ))
                    |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket in
  let contract_address1 = make_contract_address "test" in
  let table = Ticket_table.empty in
  let argument =
    Bytes.concat Bytes.empty
      [
        i64 20L;
        Ticket_handle.to_bytes handle;
        Address.to_string addr |> Bytes.of_string (* 67 *);
      ] in
  let table =
    Ticket_table.unsafe_deposit_ticket table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let module M =
  (val Contract_context.make ~source:addr ~sender:addr ~table
         ~contracts_table:(fun _ -> None)
         ~self:contract_address1 ~tickets:[handle])
  in
  let module Ctx = Wasm_vm.Ffi.Make (M) in
  let _, ops = invoke ~custom:Ctx.custom ~storage ~argument code in
  let x = M.get_ops ops |> Result.get_ok |> List.hd in
  Alcotest.(check Testables.contract_operation)
    "Same"
    (Contract_context.Contract_operation.Transfer
       { ticket; amount = Amount.of_int 10; destination = addr })
    x

let test_ticket_own_dup () =
  let open Core_deku in
  let code =
    {|
                  (module
                    (import "env" "syscall" (func $syscall (param i64) (result i32)))
                    (memory (export "memory") 1)
                    (func (export "main")  (param i32) (result i64 i64 i64)
                      i32.const 21
                      i32.const 5
                      i32.store
                      i32.const 26
                      i32.const 0
                      i32.store
                      i64.const 21 
                      call $syscall
                      i32.const 5
                      i32.store
                      i32.const 26
                      i32.const 0
                      i32.store
                      i64.const 21 
                      call $syscall
                      i64.extend_i32_s
                      (i64.const 20)
                      (i64.const 8)

                      ))
                |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket in
  let argument = handle |> Ticket_handle.to_bytes in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.unsafe_deposit_ticket table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let module M =
  (val Contract_context.make ~source:addr ~sender:addr ~table
         ~contracts_table:(fun _ -> None)
         ~self:(make_contract_address "test")
         ~tickets:[handle])
  in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore
        (let module Ctx = Wasm_vm.Ffi.Make (M) in
        invoke ~custom:Ctx.custom ~storage ~argument code
        |> fst
        |> Ticket_handle.of_bytes
        |> Option.get))

let test_extern_bindings () =
  let code =
    {|
          (module
            (import "env" "syscall" (func $syscall (param i64) (result i32)))
            (memory (export "memory") 1)
            (func (export "main")  (param i32) (result i64 i64 i64)
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
              (i64.const 45)
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
  let storage, _ = invoke ~custom ~storage ~argument code in
  let storage = Bytes.get_int64_le storage 0 in
  Alcotest.(check int64) "Same" 43L storage;
  Alcotest.(check bool) "Called" true !called

let test =
  let open Alcotest in
  ( "Runtime",
    [
      test_case "Simple invocation" `Quick test_simple_invocation;
      test_case "External calls" `Quick test_extern_bindings;
      test_case "Tickets ownership" `Quick test_ticket_own;
      test_case "Tickets ownership dup fails" `Quick test_ticket_own_dup;
      test_case "Tickets transfer: implicit -> originated -> invoke originated"
        `Quick test_ticket_send_twice;
      test_case "Tickets transfer: implicit -> originated -> implicit" `Quick
        test_ticket_send_implicit;
      test_case "Tickets join" `Quick test_ticket_join;
      test_case "Tickets split" `Quick test_ticket_split;
    ] )
