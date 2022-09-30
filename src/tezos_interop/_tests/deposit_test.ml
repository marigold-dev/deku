open Deku_crypto
open Deku_tezos
open Deku_tezos_interop
open Deku_stdlib

let secret =
  Secret.of_b58 "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
  |> Option.get

let rpc_node = Uri.of_string "http://localhost:20000"

(* We can use a random discovery contract address *)
let discovery_contract =
  Address.of_string "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton" |> Option.get

let main consensus_contract =
  let t =
    Tezos_interop.make ~rpc_node ~secret ~consensus_contract ~discovery_contract
      ~required_confirmations:2
  in

  let () =
    let open Tezos_interop in
    Consensus.listen_operations
      ~on_operation:(fun { hash = _; transactions } ->
        let is_deposit =
          transactions
          |> List.exists (fun transaction ->
                 match transaction with
                 | Consensus.Deposit _ -> true
                 | _ -> false)
        in
        if is_deposit then
          print_endline "👍 Receive a deposit on the consensus contract";
        exit 0)
      t
  in
  Lwt_main.run (Lwt_unix.sleep 10.0);
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
