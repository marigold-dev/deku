open Cmdliner
open Helpers
open Sandbox_helpers
open Feather
open Crypto
open Tezos
open Protocol

let tezos_client_update_config rpc_address =
  print_endline "update config";
  tezos_client ["--endpoint"; Uri.to_string rpc_address; "config"; "update"]

let import_secret rpc_address alias secret =
  print_endline "import secret";
  tezos_client
    [
      "--endpoint";
      Uri.to_string rpc_address;
      "import";
      "secret";
      "key";
      alias;
      secret;
      "--force";
    ]

let is_node_bootstrapped rpc_address =
  print_endline "bootstrapped";
  tezos_client ["--endpoint"; Uri.to_string rpc_address; "bootstrapped"]
  |> Result.map (String.split_on_char '\n')
  |> Result.map List.rev
  |> Result.map (fun list -> List.nth_opt list 0)
  |> Result.map
       (Option.to_result ~none:"No output from tezos-client bootstrapped")
  |> Result.join
  |> Result.map (String.starts_with ~prefix:"Node is bootstrapped")
  |> Result.fold ~ok:(fun _ -> true) ~error:(fun _ -> false)

let setup_identity i =
  let folder = Format.sprintf "data/%d" i in
  process "mkdir" ["-p"; folder] |> run;
  deku_node
    [
      "setup-identity";
      folder;
      "--uri";
      Format.sprintf "http://localhost:444%d" i;
    ]
  |> run;
  let%ok key =
    deku_node ["self"; folder]
    |. grep "key:"
    |. process "awk" ["{ print $2 }"]
    |> collect stdout
    |> Wallet.of_string
    |> Option.to_result ~none:"error in key parsing" in
  let%ok address =
    deku_node ["self"; folder]
    |. grep "address"
    |. process "awk" ["{ print $2 }"]
    |> collect stdout
    |> Key_hash.of_string
    |> Option.to_result ~none:"error in address parsing" in
  let uri =
    deku_node ["self"; folder]
    |. grep "uri:"
    |. process "awk" ["{ print $2 }"]
    |> collect stdout in
  Ok (i, key, uri, address)

let make_consensus_storage identities =
  identities
  |> List.map (fun (_, _, _, address) ->
         Format.sprintf "(\"%s\" : key_hash)" (Key_hash.to_string address))
  |> String.concat ";"
  |> Format.sprintf
       "{root_hash = { current_block_hash = 0x;current_block_height = \
        0;current_state_hash = 0x;current_handles_hash = 0x;current_validators \
        = [%s]; }; \n\
       \          vault = {\n\
       \            known_handles_hash = (Big_map.empty : \
        vault_known_handles_hash_set); used_handles = (Big_map.empty : \
        vault_used_handle_set); vault = (Big_map.empty : vault); } }"

let make_discovery_storage identities =
  identities
  |> List.map (fun (_, _, uri, address) ->
         Format.sprintf "((\"%s\" : key_hash), (0, \"%s\"))"
           (Key_hash.to_string address)
           uri)
  |> String.concat ";"
  |> Format.sprintf "Big_map.literal [%s]"

let make_trusted_validator_membership_change_json identities =
  let open Consensus.Trusted_validators_membership_change in
  let json =
    List.map (fun (_, _, _, address) -> { action = Add; address }) identities
    |> [%to_yojson: t list] in
  List.iter
    (fun (i, _, _, _) ->
      let path_to_file =
        Format.sprintf "data/%d/trusted-validator-membership-change.json" i
      in
      Yojson.Safe.to_file path_to_file json)
    identities

let setup_tezos rpc_node tezos_secret consensus_address discovery_address
    identity =
  let i, _, _, _ = identity in
  let folder = Format.sprintf "data/%i" i in
  deku_node
    [
      "setup-tezos";
      folder;
      "--tezos_consensus_contract";
      Address.to_string consensus_address;
      "--tezos_discovery_contract";
      Address.to_string discovery_address;
      "--tezos_rpc_node";
      Uri.to_string rpc_node;
      "--tezos_secret";
      tezos_secret;
      "--unsafe_tezos_required_confirmations";
      "3";
    ]
  |> run_res ~error:"error in deku-cli setup-tezos"

let setup validators (rpc_address : Uri.t) =
  (* FIXME: this relative path seems suspicious - does it work if you move directories? *)
  let consensus = "./src/tezos_interop/consensus.mligo" in
  let discovery = "./src/tezos_interop/discovery.mligo" in
  let secret = "edsk4TxW4UvCXFZrB5ifMx83PAUECKLUB95ecm1Lp4GbE8ZEeE3T1g" in
  validators
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;
  (* setup tezos-client *)
  let%assert () =
    ("the tezos node is not bootstrapped", is_node_bootstrapped rpc_address)
  in
  print_endline "foobar";
  let%ok _ = tezos_client_update_config rpc_address in
  let%ok _ =
    import_secret rpc_address "myWallet"
      (Format.sprintf "unencrypted:%s" secret) in

  (* setup write indentity.json to file system *)
  let%ok identities = validators |> List.map_ok setup_identity in

  (* deploy smart contracts *)
  let consensus_storage = make_consensus_storage identities in
  let discovery_storage = make_discovery_storage identities in
  let%ok consensus_address =
    deploy_contract ~wait:(Some 1) rpc_address "consensus" consensus
      consensus_storage "myWallet" in
  let%ok discovery_address =
    deploy_contract ~wait:(Some 1) rpc_address "discovery" discovery
      discovery_storage "myWallet" in

  (* setup tezos informations *)
  make_trusted_validator_membership_change_json identities;
  identities
  |> List.map_ok
       (setup_tezos rpc_address secret consensus_address discovery_address)

let setup nodes rpc_address =
  let validators = make_validators nodes in
  let%ok _validators = setup validators rpc_address in
  Ok ()

open Cmdliner_helpers

let term =
  let open Term in
  const setup $ nodes $ rpc_address

let info =
  let doc =
    "Does the following: it starts a Tezos sandbox network with Flextesa, then \
     it generates a new validator indentities and it deploys a new contract to \
     the Tezos sandbox configured to use these validators." in
  Cmd.info "setup" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
