let () =
  Test_ledger.run ();
  Test_operation.run ();
  Test_protocol.run ();
  Test_operation_hash.run ()
