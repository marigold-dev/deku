open Deku_crypto
open Deku_tezos
open Deku_tezos_interop
open Deku_consensus
open Deku_stdlib

let secret =
  Ed25519.Secret.of_b58 "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
  |> Option.get

let secret = Secret.Ed25519 secret
let rpc_node = Uri.of_string "http://localhost:20000"

let main consensus_contract =
  let consensus_contract = Address.of_string consensus_contract |> Option.get in

  (* We can use a random KT1 for the discovery contract since it's not used in this test *)
  let discovery_contract =
    Address.of_string "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton" |> Option.get
  in

  let t =
    Tezos_interop.make ~rpc_node ~secret ~consensus_contract ~discovery_contract
      ~required_confirmations:2
  in

  let (Block.Block { payload_hash; level; _ }) = Fixme_name.some_block in
  let () =
    Lwt.async (fun () ->
        let%await () =
          Tezos_interop.Consensus.commit_state_hash ~block_level:level
            ~block_payload_hash:payload_hash
            ~state_hash:Fixme_name.state_root_hash
            ~withdrawal_handles_hash:Fixme_name.withdrawal_handles_hash
            ~signatures:Fixme_name.some_block_keys_and_signatures
            ~validators:Fixme_name.validators t
        in
        let%await () = Lwt_unix.sleep 1. in
        failwith "Should have exited successfully before now")
  in
  let () =
    Tezos_interop.Consensus.listen_operations
      ~on_operation:(fun { hash; transactions = _ } ->
        Format.printf "👍 State root update successful. Operation hash: %s\n%!"
          (Tezos_operation_hash.to_b58 hash);
        exit 0)
      t
  in
  Lwt_main.run (Lwt_unix.sleep 10.0);
  print_endline "👎 State root hash not updated";
  exit 1

open Cmdliner

let info =
  let doc = "TODO:" in
  Cmd.info "deku-tezos-integration-test" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let consensus_contract =
    let open Arg in
    let docv = "consensus-contract" in
    let doc = "TODO:" in
    let env = Cmd.Env.info "DEKU_CONSENSUS_CONTRACT" in
    required
    & opt (some string) None
    & info [ "consensus-contract" ] ~doc ~docv ~env
  in
  let open Term in
  const main $ consensus_contract

let _ = Cmd.eval ~catch:true @@ Cmd.v info term
