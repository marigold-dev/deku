open Helpers
open Crypto
open Node
open State
open Protocol
open Cmdliner
open Core
open Bin_common

let () = Printexc.record_backtrace true
let read_validators ~node_folder =
  Files.Validators.read ~file:(node_folder ^ "/validators.json")
let read_identity ~node_folder =
  Files.Identity.read ~file:(node_folder ^ "/identity.json")
let write_identity ~node_folder =
  Files.Identity.write ~file:(node_folder ^ "/identity.json")
let write_interop_context ~node_folder =
  Files.Interop_context.write ~file:(node_folder ^ "/tezos.json")
let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]
let validators_uris node_folder =
  let%await validators = read_validators ~node_folder in
  validators |> List.map snd |> await
let make_filename_from_address wallet_addr_str =
  Printf.sprintf "%s.tzsidewallet" wallet_addr_str
let exits =
  Term.default_exits
  @
  let open Term in
  [exit_info 1 ~doc:"expected failure (might not be a bug)"]
let lwt_ret p =
  let open Term in
  ret (const Lwt_main.run $ p)
let wallet =
  let parser file =
    let non_dir_file =
      let open Arg in
      conv_parser non_dir_file in
    match
      (non_dir_file file, non_dir_file (make_filename_from_address file))
    with
    | Ok file, _
    | _, Ok file ->
      Ok file
    | _ -> Error (`Msg "Expected path to wallet") in
  let printer =
    let open Arg in
    conv_printer non_dir_file in
  let open Arg in
  conv (parser, printer)

let contract_code_path =
  let parser file =
    let non_dir_file = Arg.(conv_parser non_dir_file) in
    match
      (non_dir_file file, non_dir_file (make_filename_from_address file))
    with
    | Ok file, _
    | _, Ok file ->
      Ok file
    | _ -> Error (`Msg "Expected path to contract JSON") in

  let printer = Arg.(conv_printer non_dir_file) in
  Arg.(conv (parser, printer))

let edsk_secret_key =
  let parser key =
    match Crypto.Secret.of_string key with
    | Some key -> Ok key
    | _ -> Error (`Msg "Expected EDSK secret key") in
  let printer ppf key = Format.fprintf ppf "%s" (Crypto.Secret.to_string key) in
  let open Arg in
  conv (parser, printer)
let uri =
  let parser uri = Ok (uri |> Uri.of_string) in
  let printer ppf uri = Format.fprintf ppf "%s" (uri |> Uri.to_string) in
  let open Arg in
  conv (parser, printer)
let address_implicit =
  let parser string =
    Option.bind (Address.of_string string) Address.to_key_hash
    |> Option.to_result ~none:(`Msg "Expected a wallet address.") in
  let printer fmt wallet =
    Format.fprintf fmt "%s" (wallet |> Key_hash.to_string) in
  let open Arg in
  conv (parser, printer)

let address =
  let parser string =
    Address.of_string string
    |> Option.to_result ~none:(`Msg "Expected a valid Deku address.") in
  let printer fmt wallet =
    Format.fprintf fmt "%s" (wallet |> Address.to_string) in
  let open Arg in
  conv (parser, printer)
let address_tezos_interop =
  let parser string =
    string
    |> Tezos.Address.of_string
    |> Option.to_result ~none:(`Msg "Expected a wallet address.") in
  let printer fmt address =
    Format.fprintf fmt "%s" (Tezos.Address.to_string address) in
  let open Arg in
  conv (parser, printer)
let amount =
  let parser string =
    let%ok int =
      int_of_string_opt string
      |> Option.to_result ~none:(`Msg "Expected an amount") in
    try Ok (Amount.of_int int) with
    | _exn -> Error (`Msg "Expected an amount above zero") in
  let printer fmt amount = Format.fprintf fmt "%d" (Amount.to_int amount) in
  let open Arg in
  conv ~docv:"A positive amount" (parser, printer)
let tezos_required_confirmations =
  let msg = "Expected an integer greater than 0" in
  let parser string =
    match int_of_string_opt string with
    | Some int when int > 0 -> Ok int
    | Some _
    | None ->
      Error (`Msg msg) in
  let printer fmt int = Format.fprintf fmt "%d" int in
  let open Arg in
  conv ~docv:"An integer greater than 0" (parser, printer)
let ticket =
  let parser string =
    Tezos.Ticket_id.of_string string
    |> Option.to_result ~none:(`Msg "Expected a ticket") in
  let printer fmt ticket =
    Format.fprintf fmt "%S" (Tezos.Ticket_id.to_string ticket) in
  let open Arg in
  conv ~docv:"A ticket" (parser, printer)
let hash =
  let parser string =
    BLAKE2B.of_string string
    |> Option.to_result ~none:(`Msg "Expected a 256bits BLAKE2b hash.") in
  let printer fmt wallet = Format.fprintf fmt "%s" (BLAKE2B.to_string wallet) in
  let open Arg in
  conv (parser, printer)
let info_create_wallet =
  let doc =
    "Creates a wallet file. The wallet file's filename is its address. The \
     wallet file contains the private uri corresponding to that address." in
  Term.info "create-wallet" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
let create_wallet () =
  let secret, _key, key_hash = Key_hash.make_ed25519 () in
  let address_string = Address.to_string (key_hash |> Address.of_key_hash) in
  let file = make_filename_from_address address_string in
  let%await () =
    Files.Wallet.write { priv_key = secret; address = key_hash } ~file in
  await (`Ok ())
let create_wallet =
  let open Term in
  lwt_ret (const create_wallet $ const ())
let info_create_transaction =
  let doc =
    Printf.sprintf
      "Submits a transaction to the sidechain. The transaction will be \
       communicated to all known validators to be included in the next block. \
       If the path to the wallet file corresponding to the sending address is \
       not provided, a wallet file with the correct filename (%s) must be \
       present in the current working directory"
      (make_filename_from_address "address") in
  Term.info "create-transaction" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man
let create_transaction node_folder sender_wallet_file received_address amount
    ticket =
  let open Networking in
  let%await validators_uris = validators_uris node_folder in
  let validator_uri = List.hd validators_uris in
  let%await block_level_response = request_block_level () validator_uri in
  let block_level = block_level_response.level in
  let%await wallet = Files.Wallet.read ~file:sender_wallet_file in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:wallet.priv_key ~nonce:0l
      ~block_height:block_level
      ~data:
        (Core.User_operation.make
           ~sender:(Address.of_key_hash wallet.address)
           (Transaction { destination = received_address; amount; ticket }))
  in
  let%await identity = read_identity ~node_folder in
  let%await () =
    Networking.request_user_operation_gossip
      { user_operation = transaction }
      identity.uri in
  Format.printf "operation.hash: %s\n%!" (BLAKE2B.to_string transaction.hash);
  Lwt.return (`Ok ())

let info_originate_contract =
  let doc =
    "Originates a contract. Contract origination will be communicated to all \
     known validators to be included in the next block." in
  Term.info "originate-contract" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

let originate_contract node_folder contract_json initial_storage
    sender_wallet_file ticket amount =
  let open Networking in
  let module SM = Core.Smart_contracts in
  let%await validators_uris = validators_uris node_folder in
  let validator_uri = List.hd validators_uris in
  let%await block_level_response = request_block_level () validator_uri in
  let block_level = block_level_response.level in
  let%await wallet = Files.Wallet.read ~file:sender_wallet_file in

  let contract_program =
    contract_json
    |> Yojson.Safe.from_file
    |> SM.Raw.Script.of_yojson
    |> Result.get_ok in
  let initial_storage =
    initial_storage
    |> Yojson.Safe.from_file
    |> SM.Raw.Value.of_yojson
    |> Result.get_ok in
  let origination =
    SM.Origination_payload.make_lambda ~code:contract_program
      ~storage:initial_storage
    |> Result.get_ok in
  let origination_op =
    User_operation.Contract_origination
      { to_originate = origination; ticket; amount } in
  let originate_contract_op =
    Protocol.Operation.Core_user.sign ~secret:wallet.priv_key ~nonce:0l
      ~block_height:block_level
      ~data:
        (User_operation.make
           ~sender:(Address.of_key_hash wallet.address)
           origination_op) in
  let%await identity = read_identity ~node_folder in
  let%await () =
    Networking.request_user_operation_gossip
      {
        Networking.User_operation_gossip.user_operation = originate_contract_op;
      }
      identity.uri in
  Lwt.return (`Ok ())

let originate_contract =
  let folder_node =
    let docv = "folder_node" in
    let doc = "The folder where the node lives." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv) in
  let contract_json =
    let doc =
      "The path to the JSON output of compiling the LIGO contract to Lambda."
    in
    let open Arg in
    required
    & pos 1 (some contract_code_path) None
    & info [] ~docv:"contract" ~doc in

  let ticket =
    let doc = "The ticket to be trasnsacted." in
    let open Arg in
    required & pos 4 (some ticket) None & info [] ~docv:"ticket" ~doc in
  let initial_storage =
    let doc = "The string containing initial storage for Lambdavm" in
    let open Arg in
    required & pos 2 (some string) None & info [] ~docv:"initial_storage" ~doc
  in
  let address_from =
    let doc =
      "The sending address, or a path to a wallet. If a bare sending address \
       is provided, the corresponding wallet is assumed to be in the working \
       directory." in
    let env = Arg.env_var "SENDER" ~doc in
    Arg.(required & pos 3 (some wallet) None & info [] ~env ~docv:"sender" ~doc)
  in
  let amount =
    let doc = "The amount to be transferred." in
    let env = Arg.env_var "TRANSFER_AMOUNT" ~doc in
    let open Arg in
    required & pos 5 (some amount) None & info [] ~env ~docv:"amount" ~doc in
  Term.(
    lwt_ret
      (const originate_contract
      $ folder_node
      $ contract_json
      $ initial_storage
      $ address_from
      $ ticket
      $ amount))

let folder_node =
  let docv = "folder_node" in
  let doc = "The folder where the node lives." in
  let open Arg in
  required & pos 0 (some string) None & info [] ~doc ~docv
let create_transaction =
  let address_from =
    let doc =
      "The sending address, or a path to a wallet% If a bare sending address \
       is provided, the corresponding wallet is assumed to be in the working \
       directory." in
    let env = Arg.env_var "SENDER" ~doc in
    let open Arg in
    required & pos 1 (some wallet) None & info [] ~env ~docv:"sender" ~doc in
  let address_to =
    let doc = "The receiving address." in
    let env = Arg.env_var "RECEIVER" ~doc in
    let open Arg in
    required & pos 2 (some address) None & info [] ~env ~docv:"receiver" ~doc
  in
  let amount =
    let doc = "The amount to be transferred." in
    let env = Arg.env_var "TRANSFER_AMOUNT" ~doc in
    let open Arg in
    required & pos 3 (some amount) None & info [] ~env ~docv:"amount" ~doc in
  let ticket =
    let doc = "The ticket to be transferred." in
    let open Arg in
    required & pos 4 (some ticket) None & info [] ~docv:"ticket" ~doc in
  let open Term in
  lwt_ret
    (const create_transaction
    $ folder_node
    $ address_from
    $ address_to
    $ amount
    $ ticket)
let info_withdraw =
  let doc = Printf.sprintf "Submits a withdraw to the sidechain." in
  Term.info "withdraw" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
let withdraw node_folder sender_wallet_file tezos_address amount ticket =
  let open Networking in
  let%await identity = read_identity ~node_folder in
  let%await block_level_response = request_block_level () identity.uri in
  let block_level = block_level_response.level in
  let%await wallet = Files.Wallet.read ~file:sender_wallet_file in
  let operation =
    Protocol.Operation.Core_user.sign ~secret:wallet.priv_key ~nonce:0l
      ~block_height:block_level
      ~data:
        (Core.User_operation.make
           ~sender:(Address.of_key_hash wallet.address)
           (Tezos_withdraw { owner = tezos_address; amount; ticket })) in
  let%await () =
    Networking.request_user_operation_gossip
      { user_operation = operation }
      identity.uri in
  Format.printf "operation.hash: %s\n%!" (BLAKE2B.to_string operation.hash);
  Lwt.return (`Ok ())
let withdraw =
  let folder_node =
    let docv = "folder_node" in
    let doc = "The folder where the node lives." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let address_from =
    let doc =
      "The sending address, or a path to a wallet% If a bare sending address \
       is provided, the corresponding wallet is assumed to be in the working \
       directory." in
    let env = Arg.env_var "SENDER" ~doc in
    let open Arg in
    required & pos 1 (some wallet) None & info [] ~env ~docv:"sender" ~doc in
  let tezos_address =
    let doc =
      "The address that will be used to withdraw the ticket at Tezos **only \
       KT1 and tz1**" in
    let open Arg in
    required
    & pos 2 (some address_tezos_interop) None
    & info [] ~docv:"tezos_address" ~doc in
  let amount =
    let doc = "The amount to be transacted." in
    let env = Arg.env_var "TRANSFER_AMOUNT" ~doc in
    let open Arg in
    required & pos 3 (some amount) None & info [] ~env ~docv:"amount" ~doc in
  let ticket =
    let doc = "The ticket to be trasnsacted." in
    let open Arg in
    required & pos 4 (some ticket) None & info [] ~docv:"ticket" ~doc in
  let open Term in
  lwt_ret
    (const withdraw
    $ folder_node
    $ address_from
    $ tezos_address
    $ amount
    $ ticket)
let withdraw_proof node_folder operation_hash callback =
  let open Networking in
  let%await identity = read_identity ~node_folder in
  let%await result = request_withdraw_proof { operation_hash } identity.uri in
  match result with
  | Unknown_operation ->
    let message = BLAKE2B.to_string operation_hash ^ " is unknown" in
    await (`Error (false, message))
  | Operation_is_not_a_withdraw ->
    let message = BLAKE2B.to_string operation_hash ^ " is not a withdraw" in
    await (`Error (false, message))
  | Ok { withdrawal_handles_hash; withdrawal_handle; proof } ->
    let to_hex bytes = Hex.show (Hex.of_bytes bytes) in
    Format.printf
      {|(Pair (Pair %S
            (Pair (Pair %d 0x%s) %d %S)
            %S)
      0x%s
      { %s })|}
      (Tezos.Address.to_string callback)
      (Amount.to_int withdrawal_handle.amount)
      (to_hex withdrawal_handle.ticket.data)
      withdrawal_handle.id
      (Tezos.Address.to_string withdrawal_handle.owner)
      (Tezos.Address.to_string withdrawal_handle.ticket.ticketer)
      (BLAKE2B.to_string withdrawal_handles_hash)
      (List.map
         (fun (left, right) ->
           Format.sprintf "        Pair 0x%s\n             0x%s"
             (BLAKE2B.to_string left) (BLAKE2B.to_string right))
         proof
      |> String.concat " ;\n"
      |> String.trim);
    await (`Ok ())
let info_withdraw_proof =
  let doc = "Find withdraw proof from operation hash" in
  Term.info "withdraw-proof" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
let withdraw_proof =
  let folder_dest =
    let docv = "folder_dest" in
    let doc =
      "The folder the files will be created in. The folder must exist and be \
       empty." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let operation_hash =
    let docv = "operation_hash" in
    let doc = "The operation hash used on the withdraw." in
    let open Arg in
    required & pos 1 (some hash) None & info [] ~doc ~docv in
  let contract_callback =
    let docv = "contract_callback" in
    let doc = "Contract callback to be used on the withdraw" in
    let open Arg in
    required & pos 2 (some address_tezos_interop) None & info [] ~doc ~docv
  in
  let open Term in
  lwt_ret
    (const withdraw_proof $ folder_dest $ operation_hash $ contract_callback)
let info_sign_block =
  let doc =
    "Sign a block hash and broadcast to the network manually, useful when the \
     chain is stale." in
  Term.info "sign-block" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
let sign_block node_folder block_hash =
  let%await identity = read_identity ~node_folder in
  let signature = Signature.sign ~key:identity.secret block_hash in
  let%await validators_uris = validators_uris node_folder in
  let%await () =
    let open Networking in
    broadcast_to_list
      (module Signature_spec)
      validators_uris
      { hash = block_hash; signature } in
  Lwt.return (`Ok ())
let sign_block_term =
  let folder_node =
    let docv = "folder_node" in
    let doc = "The folder where the node lives." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let block_hash =
    let doc = "The block hash to be signed." in
    let open Arg in
    required & pos 1 (some hash) None & info [] ~doc in
  let open Term in
  lwt_ret (const sign_block $ folder_node $ block_hash)
let info_produce_block =
  let doc =
    "Produce and sign a block and broadcast to the network manually, useful \
     when the chain is stale." in
  Term.info "produce-block" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
let produce_block node_folder =
  let%await identity = read_identity ~node_folder in
  let%await state = Node_state.get_initial_state ~folder:node_folder in
  let address = identity.t in
  let block =
    Block.produce ~state:state.protocol ~next_state_root_hash:None
      ~author:address ~operations:[] in
  let signature = Block.sign ~key:identity.secret block in
  let%await validators_uris = validators_uris node_folder in
  let%await () =
    let open Networking in
    broadcast_to_list
      (module Block_and_signature_spec)
      validators_uris { block; signature } in
  Format.printf "block.hash: %s\n%!" (BLAKE2B.to_string block.hash);
  Lwt.return (`Ok ())
let produce_block =
  let folder_node =
    let docv = "folder_node" in
    let doc = "The folder where the node lives." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  lwt_ret (const produce_block $ folder_node)
let ensure_folder folder =
  let%await exists = Lwt_unix.file_exists folder in
  if exists then
    let%await stat = Lwt_unix.stat folder in
    if stat.st_kind = Lwt_unix.S_DIR then
      await ()
    else
      raise (Invalid_argument (folder ^ " is not a folder"))
  else
    Lwt_unix.mkdir folder 0o700
let setup_identity node_folder uri =
  let%await () = ensure_folder node_folder in
  let identity =
    let secret, key = Crypto.Ed25519.generate () in
    let t = Key_hash.of_key (Ed25519 key) in
    { uri; t; key = Ed25519 key; secret = Ed25519 secret } in
  let%await () = write_identity ~node_folder identity in
  await (`Ok ())
let info_setup_identity =
  let doc = "Create a validator identity" in
  Term.info "setup-identity" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
let setup_identity =
  let folder_dest =
    let docv = "folder_dest" in
    let doc =
      "The folder the files will be created in. The folder must exist and be \
       empty." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let self_uri =
    let docv = "self_uri" in
    let doc = "The uri that other nodes should use to connect to this node." in
    let open Arg in
    required & opt (some uri) None & info ["uri"] ~doc ~docv in
  let open Term in
  lwt_ret (const setup_identity $ folder_dest $ self_uri)
let info_setup_tezos =
  let doc = "Setup Tezos identity" in
  Term.info "setup-tezos" ~version:"%%VERSION%%" ~doc ~exits ~man
let setup_tezos node_folder rpc_node secret consensus_contract
    required_confirmations =
  let%await () = ensure_folder node_folder in
  let context =
    let open Tezos_interop.Context in
    { rpc_node; secret; consensus_contract; required_confirmations } in
  let%await () = write_interop_context ~node_folder context in
  await (`Ok ())
let setup_tezos =
  let folder_dest =
    let docv = "folder_dest" in
    let doc =
      "The folder the files will be created in. The folder must exist and be \
       empty." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let tezos_node_uri =
    let docv = "tezos_node_uri" in
    let doc = "The uri of the tezos node." in
    let open Arg in
    required & opt (some uri) None & info ["tezos_rpc_node"] ~doc ~docv in
  let tezos_secret =
    let docv = "tezos_secret" in
    let doc = "The Tezos secret key." in
    let open Arg in
    required
    & opt (some edsk_secret_key) None
    & info ["tezos_secret"] ~doc ~docv in
  let tezos_consensus_contract_address =
    let docv = "tezos_consensus_contract_address" in
    let doc = "The address of the Tezos consensus contract." in
    let open Arg in
    required
    & opt (some address_tezos_interop) None
    & info ["tezos_consensus_contract"] ~doc ~docv in
  let tezos_required_confirmations =
    let docv = "int" in
    let doc =
      "Set the required confirmations. WARNING: Setting below default of 10 \
       can compromise security of the Deku chain." in
    let open Arg in
    value
    & opt tezos_required_confirmations 10
    & info ["unsafe_tezos_required_confirmations"] ~doc ~docv in
  let open Term in
  lwt_ret
    (const setup_tezos
    $ folder_dest
    $ tezos_node_uri
    $ tezos_secret
    $ tezos_consensus_contract_address
    $ tezos_required_confirmations)
let show_help =
  let doc = "a tool for interacting with the WIP Tezos Sidechain" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  ( (let open Term in
    ret (const (`Help (`Pager, None)))),
    Term.info "deku-cli" ~version:"v0.0.1" ~doc ~sdocs ~exits ~man )
let info_self =
  let doc = "Shows identity key and address of the node." in
  Term.info "self" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man
let self node_folder =
  let%await identity = read_identity ~node_folder in
  Format.printf "key: %s\n" (Wallet.to_string identity.key);
  Format.printf "address: %s\n" (Key_hash.to_string identity.t);
  Format.printf "uri: %s\n" (Uri.to_string identity.uri);
  await (`Ok ())

let self =
  let folder_dest =
    let docv = "folder_dest" in
    let doc = "The folder of the node." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  lwt_ret (const self $ folder_dest)
let info_add_trusted_validator =
  let doc =
    "Helps node operators maintain a list of trusted validators they verified \
     off-chain which can later be used to make sure only trusted validators \
     are added as new validators in the network." in
  Term.info "add-trusted-validator" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man
let add_trusted_validator node_folder address =
  let open Networking in
  let%await identity = read_identity ~node_folder in
  let payload =
    let open Trusted_validators_membership_change in
    { address; action = Add } in
  let payload_json_str =
    payload
    |> Trusted_validators_membership_change.payload_to_yojson
    |> Yojson.Safe.to_string in
  let payload_hash = BLAKE2B.hash payload_json_str in
  let signature = Signature.sign ~key:identity.secret payload_hash in
  let%await () =
    Networking.request_trusted_validator_membership { signature; payload }
      identity.uri in
  await (`Ok ())
let validator_address =
  let docv = "validator_address" in
  let doc = "The validator address to be added/removed as trusted" in
  let open Arg in
  required & pos 1 (some address_implicit) None & info [] ~docv ~doc
let add_trusted_validator =
  let open Term in
  lwt_ret (const add_trusted_validator $ folder_node $ validator_address)
let info_remove_trusted_validator =
  let doc =
    "Helps node operators maintain a list of trusted validators they verified \
     off-chain which can later be used to make sure only trusted validators \
     are added as new validators in the network." in
  Term.info "remove-trusted-validator" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man
let remove_trusted_validator node_folder address =
  let open Networking in
  let%await identity = read_identity ~node_folder in
  let payload =
    let open Trusted_validators_membership_change in
    { address; action = Remove } in
  let payload_json_str =
    payload
    |> Trusted_validators_membership_change.payload_to_yojson
    |> Yojson.Safe.to_string in
  let payload_hash = BLAKE2B.hash payload_json_str in
  let signature = Signature.sign ~key:identity.secret payload_hash in
  let%await () =
    Networking.request_trusted_validator_membership { signature; payload }
      identity.uri in
  await (`Ok ())
let remove_trusted_validator =
  let open Term in
  lwt_ret (const remove_trusted_validator $ folder_node $ validator_address)

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-cli"

let () =
  Term.exit
  @@ Term.eval_choice show_help
       [
         (create_wallet, info_create_wallet);
         (create_transaction, info_create_transaction);
         (originate_contract, info_originate_contract);
         (withdraw, info_withdraw);
         (withdraw_proof, info_withdraw_proof);
         (sign_block_term, info_sign_block);
         (produce_block, info_produce_block);
         (setup_identity, info_setup_identity);
         (setup_tezos, info_setup_tezos);
         (add_trusted_validator, info_add_trusted_validator);
         (remove_trusted_validator, info_remove_trusted_validator);
         (self, info_self);
       ]
