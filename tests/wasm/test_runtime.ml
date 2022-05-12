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
                  i32.const 41
                  i32.const 5
                  i32.store
                  i32.const 46
                  i32.const 0
                  i32.store
                  i64.const 41 
                  call $syscall
                  i64.extend_i32_s
                  (i64.const 40)
                  (i64.const 95)
                  ))
            |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket (Amount.of_int 10) in
  let argument = handle |> Ticket_handle.to_bytes in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let ctx =
    Contract_context.make_state ~source:addr ~sender:addr
      ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self:(Address.of_contract_hash (make_contract_address "test"))
      ~provided_tickets:([(ticket, Amount.of_int 10)] |> List.to_seq) in
  let imit_hanlde =
    Ticket_handle.make
      (Address.of_contract_hash (make_contract_address "test"))
      ticket (Amount.of_int 10) in
  let custom = CTX.custom ~ctx in
  let storage =
    invoke ~custom ~storage ~argument code
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
                      i32.const 81
                      i32.const 6
                      i32.store
                      i32.const 86
                      i32.const 0
                      i32.store
                      i32.const 91
                      i32.const 40
                      i32.store
                      i64.const 81 
                      call $syscall
                      i64.extend_i32_s
                      (i64.const 40)
                      (i64.const 555)
                      ))
                |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket (Amount.of_int 10) in
  let self = make_contract_address "test" in
  let contract_addr = Address.of_contract_hash self in
  let handle2 = Ticket_handle.make contract_addr ticket (Amount.of_int 10) in
  let argument =
    Bytes.concat Bytes.empty
      [handle |> Ticket_handle.to_bytes; Ticket_handle.to_bytes handle2] in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let table =
    Ticket_table.deposit table ~ticket ~destination:contract_addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let imit_hanlde =
    let tt = Ticket_handle.make contract_addr ticket (Amount.of_int 20) in
    tt in
  let custom =
    make_custom ~tickets_table
      ~tickets:(Seq.return (ticket, Amount.of_int 10))
      ~self:(Address.of_contract_hash self)
      ~sender:addr ~source:addr in
  let storage =
    invoke ~custom ~storage ~argument code
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
                             i32.const 41
                             i32.const 4
                             i32.store
                             i32.const 46
                             i32.const 0
                             i32.store
                             i32.const 51
                             i64.const 5
                             i64.store
                             i32.const 60
                             i64.const 5
                             i64.store
                             i64.const 41
                             call $syscall
                             i32.const 41 
                             i32.sub
                             i32.const 5
                             i32.store
                             i32.const 5
                             i32.const 41
                             i32.store
                             i64.const 0
                             call $syscall
                             i32.const 41 
                             i32.add
                             i32.const 5
                             i32.store
                             i32.const 46
                             i32.const 81
                             i32.store
                             i64.const 41
                             call $syscall
                             i32.const 41 
                             i32.sub
                             i64.extend_i32_s
                             (i64.const 81)
                             (i64.const 555)
                             ))
                       |}
  in

  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket (Amount.of_int 10) in
  let argument = handle |> Ticket_handle.to_bytes in
  let self = make_contract_address "test" in
  let make_custom' =
    make_custom ~self:(Address.of_contract_hash self) ~sender:addr ~source:addr
  in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.tickets table ~sender:(Address.of_contract_hash self) in
  let imit_hanlde =
    Ticket_handle.make
      (Address.of_contract_hash (make_contract_address "test"))
      ticket (Amount.of_int 5) in
  let imit_hanlde = (imit_hanlde, imit_hanlde) in
  let custom =
    make_custom' ~tickets:(Seq.return (ticket, Amount.of_int 10)) ~tickets_table
  in
  let storage = invoke ~custom ~storage ~argument code |> fst in
  let first, second =
    ( Bytes.sub storage 0 40 |> Ticket_handle.of_bytes |> Option.get,
      Bytes.sub storage 41 40 |> Ticket_handle.of_bytes |> Option.get ) in
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
                            i32.const 87
                            i32.const 8
                            i32.store
                            i32.const 92
                            i32.const 0
                            i32.store
                            i32.const 97
                            i32.const 8
                            i32.store
                            i32.const 102
                            i64.const 10
                            i64.store
                            i32.const 111
                            i32.const 48
                            i32.store
                            i64.const 87
                            call $syscall
                            i64.extend_i32_s
                            (i64.const 0)
                            i32.const 102
                            i32.const 1
                            i32.store
                            i32.const 107
                            i32.const 1
                            i32.store
                            (i64.const 102)
                            ))
                      |}
  in
  let code2 =
    {|
                        (module
                          (import "env" "syscall" (func $syscall (param i64) (result i32)))
                          (memory (export "memory") 1)
                          (func (export "main")  (param i32) (result i64 i64 i64)
                            i32.const 41
                            i32.const 5
                            i32.store
                            i32.const 46
                            i32.const 0
                            i32.store
                            i64.const 41
                            call $syscall
                            i64.extend_i32_s
                            (i64.const 40)
                            (i64.const 99)
                            ))
                      |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket (Amount.of_int 10) in
  let contract_address1 = make_contract_address "test" in
  let contract_address2 = make_contract_address "test2" in
  let table = Ticket_table.empty in
  let argument =
    Bytes.concat Bytes.empty
      [
        i64 40L;
        Ticket_handle.to_bytes handle;
        Address.to_string (Address.of_contract_hash contract_address2)
        |> Bytes.of_string
        (* 67 *);
      ] in

  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, table =
    Ticket_table.tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let ctx =
    Contract_context.make_state ~source:addr ~sender:addr
      ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self:(Address.of_contract_hash contract_address1)
      ~provided_tickets:(Seq.return (ticket, Amount.of_int 10)) in
  let _, ops = invoke ~custom:(CTX.custom ~ctx) ~storage ~argument code in
  let tickets, ops = ctx#finalize ops |> Result.get_ok in
  let ops = ops |> List.hd in
  Alcotest.(check Testables.contract_operation)
    "Same"
    (Contract_operation.Invoke
       {
         tickets = [(ticket, Amount.of_int 10)];
         destination = Address.of_contract_hash contract_address2;
         param =
           Ticket_handle.to_bytes
             (Ticket_handle.make
                (Address.of_contract_hash contract_address1)
                ticket (Amount.of_int 10));
       })
    ops;
  let table =
    Ticket_table.update_tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test"))
      ~tickets in
  let tickets, param =
    match ops with
    | Contract_operation.Invoke { destination = _; param; tickets } ->
      (tickets, param)
    | _ -> failwith "wrong op" in
  let tickets_table, _table =
    Ticket_table.tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let ctx =
    Contract_context.make_state
      ~sender:(Address.of_contract_hash contract_address1)
      ~contract_owned_tickets:tickets_table
      ~self:(Address.of_contract_hash contract_address2)
      ~provided_tickets:
        (Seq.return
           (let ticket, amount = List.hd tickets in
            (ticket, amount)))
      ~source:addr
      ~get_contract_opt:(fun _ -> None) in
  let storage =
    invoke ~custom:(CTX.custom ~ctx) ~storage ~argument:param code2
    |> fst
    |> Ticket_handle.of_bytes
    |> Option.get in
  let imit_hanlde =
    Ticket_handle.make
      (Address.of_contract_hash contract_address2)
      ticket (Amount.of_int 10) in
  Alcotest.(check Testables.ticket_handle) "Same" imit_hanlde storage

let test_ticket_send_implicit () =
  let open Core_deku in
  let code =
    {|
    (module
    (import "env" "syscall" (func $syscall (param i64) (result i32)))
    (memory (export "memory") 1)
    (func (export "main")  (param i32) (result i64 i64 i64)
      i32.const 77
      i32.const 8
      i32.store
      i32.const 82
      i32.const -1
      i32.store
      i32.const 87
      i32.const 0
      i32.store
      i32.const 92
      i64.const 10
      i64.store
      i32.const 101
      i32.const 40
      i32.store
      i64.const 77
      call $syscall
      i64.extend_i32_s
      (i64.const 0)
      i32.const 92
      i32.const 1
      i32.store 
      i32.const 97
      i32.const 1 
      i32.store
      (i64.const 92)
      ))

                          |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket (Amount.of_int 10) in
  let contract_address1 = make_contract_address "test" in
  let table = Ticket_table.empty in
  let argument =
    Bytes.concat Bytes.empty
      [
        Ticket_handle.to_bytes handle;
        Address.to_string addr |> Bytes.of_string (* 67 *);
      ] in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let ctx =
    Contract_context.make_state ~source:addr ~sender:addr
      ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self:(Address.of_contract_hash contract_address1)
      ~provided_tickets:(Seq.return (ticket, Amount.of_int 10)) in
  let _, ops = invoke ~custom:(CTX.custom ~ctx) ~storage ~argument code in
  let x = ctx#finalize ops |> Result.get_ok |> snd |> List.hd in
  Alcotest.(check Testables.contract_operation)
    "Same"
    (Contract_operation.Transfer
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
                         i32.const 41
                         i32.const 5
                         i32.store
                         i32.const 46
                         i32.const 0
                         i32.store
                         i64.const 41
                         call $syscall
                         i32.const 5
                         i32.store
                         i32.const 46
                         i32.const 0
                         i32.store
                         i64.const 41
                         call $syscall
                         i64.extend_i32_s
                         (i64.const 40)
                         (i64.const 8)

                         ))
                   |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Ticket_handle.make addr ticket (Amount.of_int 10) in
  let argument = handle |> Ticket_handle.to_bytes in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let ctx =
    Contract_context.make_state ~source:addr ~sender:addr
      ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self:(make_contract_address "test" |> Address.of_contract_hash)
      ~provided_tickets:(Seq.return (ticket, Amount.of_int 10)) in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore
        (invoke ~custom:(CTX.custom ~ctx) ~storage ~argument code
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
      test_case "Tickets join" `Quick test_ticket_join;
      test_case "Tickets split" `Quick test_ticket_split;
      test_case "Tickets transfer: implicit -> originated -> invoke originated"
        `Quick test_ticket_send_twice;
      test_case "Tickets transfer: implicit -> originated -> implicit" `Quick
        test_ticket_send_implicit;
    ] )
