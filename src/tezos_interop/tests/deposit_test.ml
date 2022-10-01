open Deku_crypto
open Deku_tezos_interop
open Deku_stdlib

let secret =
  Secret.of_b58 "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
  |> Option.get

let rpc_node = Uri.of_string "http://localhost:20000"

let main consensus_contract =
  let wait sw =
    Tezos_interop.start ~sw ~rpc_node ~secret ~consensus_contract
      ~on_operation:(fun { hash = _; transactions } ->
        let is_deposit =
          transactions
          |> List.exists (fun transaction ->
                 match transaction with
                 | Tezos_interop.Deposit _ -> true
                 | _ -> false)
        in
        if is_deposit then
          print_endline "👍 Receive a deposit on the consensus contract";
        exit 0)
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let _interop = wait sw in
  Eio.Time.sleep clock 1.0;
  print_endline "👎 Deposit wasn't seen";
  exit 1

open Cmdliner

let info =
  let doc =
    "Tests if the tezos interop can listen to deposit transaction on the \
     consensus contract."
  in
  Cmd.info "deku-deposit-test" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ Common.consensus_contract

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
