open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers

let tear_down nodes =
  make_validators nodes
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;
  Ok ()

let term =
  let open Term in
  const tear_down $ nodes

let info =
  let doc = "Stops the Tezos node and destroys the Deku state." in
  Cmd.info "tear-down" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
