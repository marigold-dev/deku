open Cmdliner

let create_vm_mock_transaction () = print_endline "create a vm operation"

let () =
  let cmd =
    Cmd.group (Cmd.info "deku-cli")
    [ Create_custom_transaction.cmd; Create_mock_transaction.cmd; Generate_identity.cmd ]
  in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
