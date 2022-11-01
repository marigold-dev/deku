open Cmdliner

let () =
  let cmd =
    Cmd.group (Cmd.info "deku-cli")
      [ Submit_transaction.cmd; Generate_identity.cmd ]
  in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
