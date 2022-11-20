let () =
  let open Alcotest in
  run "Wasm-vm" ~and_exit:false ~verbose:true
    [
      ( "Basic Vm tests",
        [
          test_case "Originate/invoke increment" `Quick Increment_test.test;
          test_case "Originate/invoke tickets" `Quick Ticket_test.test;
          (* Decookie.decookie_test; *)
        ] );
    ]

(*
           test_case "Originate/Invoke increment" `Quick (fun () ->
               let open Alcotest in
               let open Ocaml_wasm_vm in
               let addr = new_address () in
               let x =
                 Env.execute
                   ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
                   ~tickets:[]
                   Env.
                     {
                       source = addr;
                       sender = addr;
                       ledger = Deku_ledger.Ledger.initial;
                       state = State.empty;
                       ticket_table = Ticket_table.init [];
                     }
                   ~operation:originate
               in
               let state = Result.get_ok x in
               let (State_entry.Entry { storage; _ }) =
                 State.fetch_contract state.state
                   Deku_ledger.(
                     Contract_address.of_user_operation_hash
                       (Deku_crypto.BLAKE2b.hash "tutturu"))
               in
               (check bool) "Invoke" true (storage = Int Z.zero);
               let x =
                 Env.execute
                   ~operation_hash:(Deku_crypto.BLAKE2b.hash "tutturu")
                   ~tickets:[]
                   Env.
                     {
                       source = addr;
                       sender = addr;
                       ledger = Deku_ledger.Ledger.initial;
                       state = state.state;
                       ticket_table = Ticket_table.init [];
                     }
                   ~operation:invoke
               in
               let state = Result.get_ok x in
               let (State_entry.Entry { storage; _ }) =
                 State.fetch_contract state.state
                   Deku_ledger.(
                     Contract_address.of_user_operation_hash
                       (Deku_crypto.BLAKE2b.hash "tutturu"))
               in
               (check bool) "example" true (storage = Int (Z.of_int 5));
               ());
               ] );
               ]
*)
