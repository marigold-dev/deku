open Helpers
open Test_helpers
open Deku_data

let test_simple_invocation () =
  let open Core_deku in
  let open Contracts in
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
  let addr = make_address () in
  let ctx, _ =
    Context.make_state ~mapping:[] ~source:addr ~sender:addr
      ~contract_owned_tickets:Seq.empty
      ~get_contract_opt:(fun _ -> None)
      ~self:(Address.of_contract_hash (make_contract_address "test"))
      ~provided_tickets:Seq.empty in
  let storage, _ = invoke ~ctx ~storage ~argument code in
  let storage = Bytes.get_int64_le storage 0 in
  Alcotest.(check int64) "Same" 43L storage

let test_ticket_own () =
  let open Core_deku in
  let open Contracts in
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
  let argument = Int32.zero |> Context.Ticket_handle.to_bytes in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.take_all_tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let ctx, _ =
    Context.make_state ~source:addr ~sender:addr ~mapping:[]
      ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self:(Address.of_contract_hash (make_contract_address "test"))
      ~provided_tickets:
        ([((ticket, Amount.of_int 10), (Int32.zero, Some Int64.zero))]
        |> List.to_seq) in
  let _storage, _ = invoke ~ctx ~storage ~argument code in
  let _, tickets, _ = ctx#finalize [] |> Result.get_ok in
  let contract_addr = Address.of_contract_hash (make_contract_address "test") in
  let final = Ticket_table.update_tickets ~sender:contract_addr table ~tickets in
  let final = Ticket_table.balance final ~sender:contract_addr ~ticket in
  let table =
    Ticket_table.deposit ~ticket ~destination:contract_addr
      ~amount:(Amount.of_int 10) table in
  let expected = Ticket_table.balance table ~sender:contract_addr ~ticket in
  Alcotest.(check (option Testables.amount)) "Same" expected final

let test_ticket_join () =
  let open Core_deku in
  let open Contracts in
  let open Context in
  let code =
    {|
                  (module
                    (import "env" "syscall" (func $syscall (param i64) (result i32)))
                    (memory (export "memory") 1)
                    (func (export "main")  (param i32) (result i64 i64 i64)
                      i32.const 10
                      i32.const 6
                      i32.store
                      i32.const 15
                      i32.const 0
                      i32.store
                      i32.const 20
                      i32.const 4
                      i32.store
                      i64.const 10
                      call $syscall
                      i32.const 5
                      i32.add
                      i32.const 5 
                      i32.store
                      i32.const 20
                      i32.const 10
                      i32.store
                      i64.const 15
                      call $syscall
                      i64.extend_i32_s
                      (i64.const 8)
                      (i64.const 555)
                      ))
                |}
  in
  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let handle = Int32.zero |> Context.Ticket_handle.to_bytes in
  let self = make_contract_address "test" in
  let contract_addr = Deku_data.Address.of_contract_hash self in
  let handle2 = Int32.one |> Context.Ticket_handle.to_bytes in
  let argument = Bytes.concat Bytes.empty [handle; handle2] in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let table =
    Ticket_table.deposit table ~ticket ~destination:contract_addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.take_all_tickets table
      ~sender:
        (Deku_data.Address.of_contract_hash (make_contract_address "test"))
  in
  let ctx, _ =
    make_custom ~tickets_table
      ~mapping:[((ticket, Amount.of_int 10), Int32.zero)]
      ~tickets:(Seq.return ((ticket, Amount.of_int 10), (Int32.one, Some 5L)))
      ~self:(Deku_data.Address.of_contract_hash self)
      ~sender:addr ~source:addr in
  let _, _ = invoke ~ctx ~storage ~argument code in
  let _, tickets, _ = ctx#finalize [] |> Result.get_ok in
  let final = Ticket_table.update_tickets ~sender:contract_addr table ~tickets in
  let final = Ticket_table.balance final ~sender:contract_addr ~ticket in
  let table =
    Ticket_table.deposit ~ticket ~destination:contract_addr
      ~amount:(Amount.of_int 10) table in
  let expected = Ticket_table.balance table ~sender:contract_addr ~ticket in
  Alcotest.(check @@ option Testables.amount) "Same" expected final

let test_ticket_split () =
  let open Core_deku in
  let open Contracts in
  let open Context in
  let code =
    {|
                               (module
                                 (import "env" "syscall" (func $syscall (param i64) (result i32)))
                                 (memory (export "memory") 1)
                                 (func (export "main")  (param i32) (result i64 i64 i64)
                                   i32.const 5
                                   i32.const 4
                                   i32.store
                                   i32.const 10
                                   i32.const 0
                                   i32.store
                                   i32.const 15
                                   i64.const 5
                                   i64.store
                                   i32.const 24
                                   i64.const 5
                                   i64.store
                                   i64.const 5
                                   call $syscall
                                   i32.const 95
                                   i32.add
                                   i32.const 5
                                   i32.store
                                   i32.const 105
                                   i32.const 5
                                   i32.store
                                   i64.const 100
                                   call $syscall
                                   i32.const 6
                                   i32.add
                                   i32.const 5
                                   i32.store
                                   i32.const 111
                                   i32.const 9
                                   i32.store
                                   i64.const 106
                                   call $syscall
                                   i32.const 5
                                   i32.sub
                                   i64.extend_i32_s
                                   (i64.const 10)
                                   (i64.const 555)
                                   ))
                             |}
  in

  let storage = Bytes.empty in
  let addr = make_address () in
  let ticket = make_ticket () in
  let argument = Int32.zero |> Ticket_handle.to_bytes in
  let self = make_contract_address "test" in
  let make_custom' =
    make_custom
      ~self:(Deku_data.Address.of_contract_hash self)
      ~sender:addr ~source:addr in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.take_all_tickets table
      ~sender:(Deku_data.Address.of_contract_hash self) in
  let ctx, _ =
    make_custom' ~mapping:[]
      ~tickets:(Seq.return ((ticket, Amount.of_int 10), (Int32.zero, Some 0L)))
      ~tickets_table in
  let _, _ = invoke ~ctx ~storage ~argument code in
  let _, tickets, _ = ctx#finalize [] |> Result.get_ok in
  let tickets = List.of_seq tickets in
  let imit_hanlde = [(ticket, Amount.of_int 5); (ticket, Amount.of_int 5)] in
  Alcotest.(check @@ list @@ pair Testables.ticket_id Testables.amount)
    "Same tickets" imit_hanlde tickets

let test_ticket_send_twice () =
  let open Core_deku in
  let open Contracts in
  let open Context in
  let code =
    {|
                                  (module
                                    (import "env" "syscall" (func $syscall (param i64) (result i32)))
                                    (memory (export "memory") 1)
                                    (func (export "main")  (param i32) (result i64 i64 i64)
                                      i32.const 57
                                      i32.const 8
                                      i32.store
                                      i32.const 62
                                      i32.const 0
                                      i32.store
                                      i32.const 67
                                      i32.const 8
                                      i32.store
                                      i32.const 72
                                      i64.const 10
                                      i64.store
                                      i32.const 81
                                      i32.const 12
                                      i32.store
                                      i64.const 57
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
  let handle = Int32.zero in
  let contract_address1 = make_contract_address "test" in
  let contract_address2 = make_contract_address "test2" in
  let table = Ticket_table.empty in
  let argument =
    Bytes.concat Bytes.empty
      [
        i64 4L;
        Ticket_handle.to_bytes handle;
        Address.to_string (Deku_data.Address.of_contract_hash contract_address2)
        |> Bytes.of_string;
      ] in

  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, table =
    Ticket_table.take_all_tickets table
      ~sender:
        (Deku_data.Address.of_contract_hash (make_contract_address "test"))
  in
  let ctx, _ =
    make_state ~source:addr ~sender:addr ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~mapping:[]
      ~self:(Deku_data.Address.of_contract_hash contract_address1)
      ~provided_tickets:
        (Seq.return ((ticket, Amount.of_int 10), (Int32.zero, Some 4L))) in
  let _, ops = invoke ~ctx ~storage ~argument code in
  let _, tickets, ops = ctx#finalize ops |> Result.get_ok in
  let ops = ops |> List.hd in
  Alcotest.(check Testables.contract_operation)
    "Same Ops"
    (Operation.Invoke
       {
         tickets = [((ticket, Amount.of_int 10), (Int32.zero, Some 0L))];
         destination = Deku_data.Address.of_contract_hash contract_address2;
         param = Ticket_handle.to_bytes Int32.zero;
       })
    ops;

  let table =
    Ticket_table.update_tickets table
      ~sender:
        (Deku_data.Address.of_contract_hash (make_contract_address "test"))
      ~tickets in
  let tickets, param =
    match ops with
    | Operation.Invoke { destination = _; param; tickets } -> (tickets, param)
    | _ -> failwith "wrong op" in
  let tickets_table, _table =
    Ticket_table.take_all_tickets table
      ~sender:
        (Deku_data.Address.of_contract_hash (make_contract_address "test"))
  in
  let ctx, _ =
    make_state ~mapping:[]
      ~sender:(Deku_data.Address.of_contract_hash contract_address1)
      ~contract_owned_tickets:tickets_table
      ~self:(Deku_data.Address.of_contract_hash contract_address2)
      ~provided_tickets:
        (Seq.return
           (let ticket, amount = List.hd tickets in
            (ticket, amount)))
      ~source:addr
      ~get_contract_opt:(fun _ -> None) in
  let _, _ = invoke ~ctx ~storage ~argument:param code2 in
  let _, tickets, _ = ctx#finalize [] |> Result.get_ok in
  let tickets = List.of_seq tickets in
  let imit_hanlde = [(ticket, Amount.of_int 10)] in
  Alcotest.(check @@ list @@ pair Testables.ticket_id Testables.amount)
    "Same" imit_hanlde tickets

let test_ticket_send_implicit () =
  let open Core_deku in
  let open Contracts in
  let code =
    {|
              (module
              (import "env" "syscall" (func $syscall (param i64) (result i32)))
              (memory (export "memory") 1)
              (func (export "main")  (param i32) (result i64 i64 i64)
                i32.const 49
                i32.const 8
                i32.store
                i32.const 54
                i32.const -1
                i32.store
                i32.const 59
                i32.const 0
                i32.store
                i32.const 64
                i64.const 10
                i64.store
                i32.const 73
                i32.const 4
                i32.store
                i64.const 49
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
  let handle = Int32.zero in
  let contract_address1 = make_contract_address "test" in
  let table = Ticket_table.empty in
  let argument =
    Bytes.concat Bytes.empty
      [
        Context.Ticket_handle.to_bytes handle;
        Address.to_string addr |> Bytes.of_string;
      ] in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.take_all_tickets table
      ~sender:(Address.of_contract_hash (make_contract_address "test")) in
  let ctx, _ =
    Context.make_state ~source:addr ~sender:addr ~mapping:[]
      ~contract_owned_tickets:tickets_table
      ~get_contract_opt:(fun _ -> None)
      ~self:(Address.of_contract_hash contract_address1)
      ~provided_tickets:
        (Seq.return ((ticket, Amount.of_int 10), (Int32.zero, Some 0L))) in
  let _, ops = invoke ~ctx ~storage ~argument code in
  let x = ctx#finalize ops |> Result.get_ok |> (fun (_, _, x) -> x) |> List.hd in
  Alcotest.(check Testables.contract_operation)
    "Same"
    (Context.Operation.Transfer
       { ticket; amount = Amount.of_int 10; destination = addr })
    x

let test_ticket_own_dup () =
  let open Core_deku in
  let open Contracts in
  let open Context in
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
  let handle = Int32.zero in
  let argument = handle |> Ticket_handle.to_bytes in
  let table = Ticket_table.empty in
  let table =
    Ticket_table.deposit table ~ticket ~destination:addr
      ~amount:(Amount.of_int 10) in
  let tickets_table, _table =
    Ticket_table.take_all_tickets table
      ~sender:
        (Deku_data.Address.of_contract_hash (make_contract_address "test"))
  in
  let ctx, _ =
    make_state ~source:addr ~sender:addr ~contract_owned_tickets:tickets_table
      ~mapping:[]
      ~get_contract_opt:(fun _ -> None)
      ~self:(make_contract_address "test" |> Deku_data.Address.of_contract_hash)
      ~provided_tickets:
        (Seq.return ((ticket, Amount.of_int 10), (Int32.zero, None))) in
  Alcotest.check_raises "Invocation error" Invocation_error (fun () ->
      ignore
        (invoke ~ctx ~storage ~argument code |> fst |> Ticket_handle.of_bytes))

let test =
  let open Alcotest in
  ( "Runtime",
    [
      test_case "Simple invocation" `Quick test_simple_invocation;
      test_case "Tickets join" `Quick test_ticket_join;
      test_case "Tickets ownership dup fails" `Quick test_ticket_own_dup;
      test_case "Tickets ownership" `Quick test_ticket_own;
      test_case "Tickets split" `Quick test_ticket_split;
      test_case "Tickets transfer: implicit -> originated -> invoke originated"
        `Quick test_ticket_send_twice;
      test_case "Tickets transfer: implicit -> originated -> implicit" `Quick
        test_ticket_send_implicit;
    ] )
