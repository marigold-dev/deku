let () =
  let open Alcotest in
  run "Wasm-vm" ~and_exit:false ~verbose:true
    [
      ( "Basic Vm tests",
        [
          test_case "Originate/invoke increment" `Quick Increment_test.test;
          test_case "Originate/invoke tickets" `Quick Ticket_test.test;
          Decookie.decookie_test;
        ] );
    ]
