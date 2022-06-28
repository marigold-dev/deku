open Feather
open Cmdliner

let curl = process "curl"

let jq = process "jq"

let current_main_chain_height ~rpc_node ~consensus_contract_address =
  let url =
    Format.sprintf "%s/chains/main/blocks/head/context/contracts/%s/storage"
      rpc_node consensus_contract_address in
  curl ["--silent"; url]
  (* TODO: we should use the Tezos_bridge module. *)
  |. jq [".args[0].args[0].args[0].args[1].int"; "-r"]
  |> collect stdout
  |> int_of_string

let check_liveness rpc_node consensus_contract_address timeout =
  let initial_height =
    current_main_chain_height ~rpc_node ~consensus_contract_address in
  let rec wait n =
    match n with
    | 0 ->
      print_endline "Chain has not made progress 😒";
      exit 1
    | _ ->
      let current_height =
        current_main_chain_height ~rpc_node ~consensus_contract_address in
      if current_height > initial_height then (
        print_endline "Chain is making progress 👍";
        exit 0)
      else (
        print_endline "Waiting for chain to make progress...";
        Unix.sleep 1;
        wait (n - 1)) in
  wait timeout

let args =
  let rpc_node =
    let docv = "rpc_node" in
    let doc = "RPC node to use check consensus progress on main chain." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let consensus_contract_address =
    let docv = "consensus_contract_address" in
    let doc = "The address of the consensus contract on the main chain" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let timeout =
    let docv = "timeout" in
    let doc = "Number of seconds to wait before failing" in
    let open Arg in
    value & opt int 60 & info ["timeout"] ~doc ~docv in
  let open Term in
  const check_liveness $ rpc_node $ consensus_contract_address $ timeout

let _ =
  Cmd.eval
  @@ Cmd.v
       (Cmd.info "check-liveness"
          ~doc:
            "Checks that the Deku chain is making progress on the mainchain by \
             polling for changes to the consensus storage.")
       args
