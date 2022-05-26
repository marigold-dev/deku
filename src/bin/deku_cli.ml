open Helpers
open Crypto
open Node
open State
open Protocol
open Cmdliner
open Core_deku
open Bin_common

let () = Printexc.record_backtrace true

let read_identity ~node_folder =
  Files.Identity.read ~file:(node_folder ^ "/identity.json")

let write_identity ~node_folder =
  Files.Identity.write ~file:(node_folder ^ "/identity.json")

let write_interop_context ~node_folder =
  Files.Interop_context.write ~file:(node_folder ^ "/tezos.json")

let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

let interop_context node_folder =
  let%await context =
    Files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
  Lwt.return
    (Tezos_interop.make ~rpc_node:context.rpc_node ~secret:context.secret
       ~consensus_contract:context.consensus_contract
       ~discovery_contract:context.discovery_contract
       ~required_confirmations:context.required_confirmations)

let validator_uris ~interop_context =
  Tezos_interop.Consensus.fetch_validators interop_context

let make_filename_from_address wallet_addr_str =
  Printf.sprintf "%s.tzsidewallet" wallet_addr_str

let exits =
  Cmd.Exit.defaults
  @ [Cmd.Exit.info 1 ~doc:"expected failure (might not be a bug)"]

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
    non_dir_file file
    |> Result.map_error (fun _ -> `Msg "Expected path to contract JSON") in
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

let address_tezos_interop =
  let parser string =
    string
    |> Tezos.Address.of_string
    |> Option.to_result ~none:(`Msg "Expected a wallet address.") in
  let printer fmt address =
    Format.fprintf fmt "%s" (Tezos.Address.to_string address) in
  let open Arg in
  conv (parser, printer)

let argument =
  let parser string = Ok (Yojson.Safe.from_string string) in
  let printer fmt arg =
    Format.fprintf fmt "%s" (Yojson.Safe.pretty_to_string arg) in
  let open Arg in
  conv ~docv:"A valid contract argument" (parser, printer)

let address =
  let parser string =
    Address.of_string string
    |> Option.to_result ~none:(`Msg "Expected a valid Deku address.") in
  let printer fmt wallet =
    Format.fprintf fmt "%s" (wallet |> Address.to_string) in
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

let vm_flavor =
  let parser string =
    (match string with
    | "Lambda" -> Some `Lambda
    | "Dummy" -> Some `Dummy
    | _ -> None)
    |> Option.to_result ~none:(`Msg "Expected a valid Vm_flavor") in
  let printer fmt flavor =
    Format.fprintf fmt "%S"
      (match flavor with
      | `Lambda -> "Lambda"
      | `Dummy -> "Dummy") in
  let open Arg in
  conv ~docv:"Vm_flavor" (parser, printer)

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
  Cmd.info "create-wallet" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

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
    (* TODO: these docs are out of sync for deku-p *)
    Printf.sprintf
      "Submits a transaction to the sidechain. The transaction will be \
       communicated to all known validators to be included in the next block. \
       If the path to the wallet file corresponding to the sending address is \
       not provided, a wallet file with the correct filename (%s) must be \
       present in the current working directory"
      (make_filename_from_address "address") in
  Cmd.info "create-transaction" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

let with_validator_uri node_folder f =
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris -> (
    let validator_uris =
      List.filter_map
        (function
          | key_hash, Some uri -> Some (key_hash, uri)
          | _ -> None)
        validator_uris in
    match validator_uris with
    | [] -> Lwt.return (`Error (false, "No validators found"))
    | validator_uri :: _ -> f validator_uri)

let create_transaction node_folder sender_wallet_file received_address amount
    ticket argument vm_flavor =
  let open Network in
  with_validator_uri node_folder @@ fun (_, validator_uri) ->
  let%await block_level_response = request_block_level () validator_uri in
  let block_level = block_level_response.level in
  let%await wallet = Files.Wallet.read ~file:sender_wallet_file in
  let operation =
    match (Address.to_key_hash received_address, argument) with
    | Some addr, None ->
      Core_deku.User_operation.make ~source:wallet.address
        (Transaction { destination = addr; amount; ticket })
    | Some _, Some _ -> failwith "can't pass an argument to implicit account"
    | None, None -> failwith "Invalid transaction"
    | None, Some arg ->
      let payload =
        match vm_flavor with
        | `Lambda -> Contract_vm.Invocation_payload.lambda_of_yojson ~arg
        | `Dummy -> Contract_vm.Invocation_payload.dummy_of_yojson ~arg in
      let arg = payload |> Result.get_ok in
      Core_deku.User_operation.make ~source:wallet.address
        (Contract_invocation
           {
             to_invoke = Address.to_contract_hash received_address |> Option.get;
             argument = arg;
           }) in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:wallet.priv_key
      ~nonce:(Crypto.Random.int32 Int32.max_int)
      ~block_height:block_level ~data:operation in

  let%await identity = read_identity ~node_folder in
  let%await () =
    Network.request_user_operation_gossip
      { user_operation = transaction }
      identity.uri in
  Format.printf "operation.hash: %s\n%!" (BLAKE2B.to_string transaction.hash);
  Lwt.return (`Ok ())

let info_create_custom_transaction =
  let doc =
    Printf.sprintf
      "Submits a transaction to Parametric Deku. The transaction will be \
       communicated to all known validators and included in the next block. If \
       the path to the wallet file corresponding to the sending address is not \
       proivded. a wallet file with the correct filename (%s) must be present \
       in the working directory"
      (make_filename_from_address "address") in
  Cmd.info "create-custom-transaction" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

(* FIXME: add way to create custom VM transaction *)
let create_custom_transaction node_folder sender_wallet_file payload =
  let open Network in
  with_validator_uri node_folder @@ fun (_, validator_uri) ->
  let%await block_level_response = request_block_level () validator_uri in
  let block_level = block_level_response.level in
  let%await wallet = Files.Wallet.read ~file:sender_wallet_file in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:wallet.priv_key
      ~nonce:(Crypto.Random.int32 Int32.max_int)
      ~block_height:block_level
      ~data:
        (Core_deku.User_operation.make ~source:wallet.address
           (Vm_transaction { payload = Yojson.Safe.from_string payload })) in
  let%await identity = read_identity ~node_folder in
  let%await () =
    request_user_operation_gossip { user_operation = transaction } identity.uri
  in
  Format.printf "operation.hash: %s\n%!" (BLAKE2B.to_string transaction.hash);
  Lwt.return (`Ok ())

let info_create_mock_transaction =
  let doc =
    Printf.sprintf
      "Reads a VM state from stdin, simulates a transaction against the VM \
       listening listening at the named pipes  specified, then prints the \
       resultant VM state to stdout." in
  Cmd.info "create-mock-transaction" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

let create_mock_transaction sender_wallet_file vm_binary_path payload =
  let rec read_all acc =
    try read_all (acc ^ read_line ()) with
    | End_of_file -> acc in
  (* TODO: not great error handling. Probably gives an ugly error message. *)
  let state =
    read_all ""
    |> Yojson.Safe.from_string
    |> External_vm.of_yojson
    |> Result.get_ok in
  let%await wallet = Files.Wallet.read ~file:sender_wallet_file in
  let payload = Yojson.Safe.from_string payload in
  (* We sign the payload just to make sure the wallet is an actual wallet.
     This adds to the realism of the test. *)
  let _transaction =
    Protocol.Operation.Core_user.sign ~secret:wallet.priv_key
      ~nonce:(Crypto.Random.int32 Int32.max_int)
      ~block_height:0L
      ~data:
        (Core_deku.User_operation.make ~source:wallet.address
           (Vm_transaction { payload })) in
  let named_pipe_path = "/tmp/deku/named_pipe" in
  let stdin, _ = Unix.pipe () in
  let _pid =
    Unix.create_process vm_binary_path
      [|vm_binary_path; named_pipe_path|]
      stdin Unix.stdout Unix.stderr in
  External_vm.start_vm_ipc ~named_pipe_path;
  External_vm.apply_vm_operation ~state ~source:wallet.address payload
  |> External_vm.to_yojson
  |> Yojson.Safe.to_string
  |> print_endline;
  (* The created process should close itself when we send the close signal. *)
  External_vm.close_vm_ipc ();
  await (`Ok ())

let info_originate_contract =
  let doc =
    "Originates a contract. Contract origination will be communicated to all \
     known validators to be included in the next block." in
  Cmd.info "originate-contract" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

let originate_contract node_folder contract_json initial_storage
    sender_wallet_file (vm_flavor : [`Dummy | `Lambda]) =
  let open Network in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris -> (
    let validator_uris =
      List.filter_map
        (function
          | key_hash, Some uri -> Some (key_hash, uri)
          | _ -> None)
        validator_uris in
    match validator_uris with
    | [] -> Lwt.return (`Error (false, "No validators found"))
    | (_, validator_uri) :: _ ->
      let%await block_level_response = request_block_level () validator_uri in
      let block_level = block_level_response.level in
      let%await wallet = Files.Wallet.read ~file:sender_wallet_file in
      let contract_program = Yojson.Safe.from_file contract_json in
      let initial_storage = Yojson.Safe.from_file initial_storage in
      let payload =
        match vm_flavor with
        | `Lambda ->
          Contract_vm.Origination_payload.lambda_of_yojson
            ~code:contract_program ~storage:initial_storage
          |> Result.get_ok
        | `Dummy ->
          let int =
            try Yojson.Safe.Util.to_int initial_storage with
            | _ -> failwith "Invalid storage fro contract" in
          Contract_vm.Origination_payload.dummy_of_yojson ~storage:int in
      let origination_op = User_operation.Contract_origination payload in
      let originate_contract_op =
        Protocol.Operation.Core_user.sign ~secret:wallet.priv_key
          ~nonce:(Crypto.Random.int32 Int32.max_int)
          ~block_height:block_level
          ~data:(User_operation.make ~source:wallet.address origination_op)
      in
      let%await identity = read_identity ~node_folder in
      let%await () =
        Network.request_user_operation_gossip
          {
            Network.User_operation_gossip.user_operation = originate_contract_op;
          }
          identity.uri in
      Format.printf "operation_hash: %s\n%!"
        (BLAKE2B.to_string originate_contract_op.hash);
      Format.printf "contract_address: %s\n%!"
        (Contract_address.of_user_operation_hash originate_contract_op.hash
        |> Contract_address.to_string);
      Lwt.return (`Ok ()))

let folder_node position =
  let docv = "folder_node" in
  let doc = "The folder where the node lives." in
  let open Arg in
  required & pos position (some string) None & info [] ~doc ~docv

let address_from position =
  let doc =
    "The sending address, or a path to a wallet% If a bare sending address is \
     provided, the corresponding wallet is assumed to be in the working \
     directory." in
  let env = Cmd.Env.info "SENDER" ~doc in
  let open Arg in
  required & pos position (some wallet) None & info [] ~env ~docv:"sender" ~doc

let originate_contract =
  let address_from =
    let doc =
      "The sending address, or a path to a wallet. If a bare address is \
       provided, the corresponding wallet is assumed to be in the working \
       directory." in
    let env = Cmd.Env.info "SENDER" ~doc in
    Arg.(required & pos 1 (some wallet) None & info [] ~env ~docv:"sender" ~doc)
  in
  let contract_json =
    let doc =
      "The path to the JSON output of compiling the contract to Lambda." in
    let open Arg in
    required
    & pos 2 (some contract_code_path) None
    & info [] ~docv:"contract" ~doc in
  let initial_storage =
    let doc = "The string containing initial storage for Lambdavm" in
    let open Arg in
    required & pos 3 (some string) None & info [] ~docv:"initial_storage" ~doc
  in
  let vm_flavor =
    let doc = "Virtual machine flavor. can be either Lambda or Dummy" in
    let env = Cmd.Env.info "VM_FLAVOR" ~doc in
    Arg.(
      Arg.value
      & opt ~vopt:`Lambda vm_flavor `Lambda
      & info ["vm_flavor"] ~env ~docv:"vm_flavor" ~doc) in
  Term.(
    lwt_ret
      (const originate_contract
      $ folder_node 0
      $ contract_json
      $ initial_storage
      $ address_from
      $ vm_flavor))

let create_transaction =
  let address_to =
    let doc = "The receiving address." in
    let env = Cmd.Env.info "RECEIVER" ~doc in
    let open Arg in
    required & pos 2 (some address) None & info [] ~env ~docv:"receiver" ~doc
  in
  let amount =
    let doc = "The amount to be transferred." in
    let env = Cmd.Env.info "TRANSFER_AMOUNT" ~doc in
    let open Arg in
    required & pos 3 (some amount) None & info [] ~env ~docv:"amount" ~doc in
  let ticket =
    let doc = "The ticket to be transferred." in
    let open Arg in
    required & pos 4 (some ticket) None & info [] ~docv:"ticket" ~doc in
  let argument =
    let doc = "Argument to be passed to transaction" in
    let open Arg in
    value @@ (opt (some argument) None & info ["arg"] ~docv:"argument" ~doc)
  in
  let vm_flavor =
    let doc = "Virtual machine flavor. can be either Lambda or Dummy" in
    let env = Cmd.Env.info "VM_FLAVOR" ~doc in
    Arg.(
      Arg.value
      & opt ~vopt:`Lambda vm_flavor `Lambda
      & info ["vm_flavor"] ~env ~docv:"vm_flavor" ~doc) in
  let open Term in
  lwt_ret
    (const create_transaction
    $ folder_node 0
    $ address_from 1
    $ address_to
    $ amount
    $ ticket
    $ argument
    $ vm_flavor)

let create_custom_transaction =
  let payload =
    let doc = "The payload as valid JSON string." in
    let env = Cmd.Env.info "PAYLOAD" ~doc in
    Arg.(
      required & pos 2 (some string) None & info [] ~env ~docv:"payload" ~doc)
  in
  let open Term in
  lwt_ret
    (const create_custom_transaction $ folder_node 0 $ address_from 1 $ payload)

let create_mock_transaction =
  let address_from =
    let doc =
      "The sending address, or a path to a wallet% If a bare sending address \
       is provided, the corresponding wallet is assumed to be in the working \
       directory." in
    let env = Cmd.Env.info "SENDER" ~doc in
    let open Arg in
    required & pos 0 (some wallet) None & info [] ~env ~docv:"sender" ~doc in
  let vm_path =
    let docv = "vm_path" in
    let doc = "Path to the binary to execute the transaction against. " in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let payload =
    let doc = "The payload as valid JSON string." in
    let env = Cmd.Env.info "PAYLOAD" ~doc in
    Arg.(
      required & pos 2 (some string) None & info [] ~env ~docv:"payload" ~doc)
  in
  let open Term in
  lwt_ret (const create_mock_transaction $ address_from $ vm_path $ payload)

let info_withdraw =
  let doc = Printf.sprintf "Submits a withdraw to the sidechain." in
  Cmd.info "withdraw" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let withdraw node_folder sender_wallet_file tezos_address amount ticket =
  let open Network in
  let%await identity = read_identity ~node_folder in
  let%await block_level_response = request_block_level () identity.uri in
  let block_level = block_level_response.level in
  let%await wallet = Files.Wallet.read ~file:sender_wallet_file in
  let operation =
    Protocol.Operation.Core_user.sign ~secret:wallet.priv_key
      ~nonce:(Crypto.Random.int32 Int32.max_int)
      ~block_height:block_level
      ~data:
        (Core_deku.User_operation.make ~source:wallet.address
           (Tezos_withdraw { owner = tezos_address; amount; ticket })) in
  let%await () =
    Network.request_user_operation_gossip
      { user_operation = operation }
      identity.uri in
  Format.printf "operation.hash: %s\n%!" (BLAKE2B.to_string operation.hash);
  Lwt.return (`Ok ())

let withdraw =
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
    let env = Cmd.Env.info "TRANSFER_AMOUNT" ~doc in
    let open Arg in
    required & pos 3 (some amount) None & info [] ~env ~docv:"amount" ~doc in
  let ticket =
    let doc = "The ticket to be trasnsacted." in
    let open Arg in
    required & pos 4 (some ticket) None & info [] ~docv:"ticket" ~doc in
  let open Term in
  lwt_ret
    (const withdraw
    $ folder_node 0
    $ address_from 1
    $ tezos_address
    $ amount
    $ ticket)

let withdraw_proof node_folder operation_hash callback =
  let open Network in
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
  Cmd.info "withdraw-proof" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let withdraw_proof =
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
    (const withdraw_proof $ folder_node 0 $ operation_hash $ contract_callback)

let info_sign_block =
  let doc =
    "Sign a block hash and broadcast to the network manually, useful when the \
     chain is stale." in
  Cmd.info "sign-block" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let sign_block node_folder block_hash =
  let%await identity = read_identity ~node_folder in
  let signature = Signature.sign ~key:identity.secret block_hash in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris ->
    let validator_uris = List.map snd validator_uris |> List.somes in
    let%await () =
      let open Network in
      broadcast_to_list
        (module Signature_spec)
        validator_uris
        { hash = block_hash; signature } in
    Lwt.return (`Ok ())

let sign_block_term =
  let block_hash =
    let doc = "The block hash to be signed." in
    let open Arg in
    required & pos 1 (some hash) None & info [] ~doc in
  let open Term in
  lwt_ret (const sign_block $ folder_node 0 $ block_hash)

let info_produce_block =
  let doc =
    "Produce and sign a block and broadcast to the network manually, useful \
     when the chain is stale." in
  Cmd.info "produce-block" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let produce_block node_folder =
  let%await identity = read_identity ~node_folder in
  let%await state = Node_state.get_initial_state ~folder:node_folder in
  let address = identity.t in
  let block =
    Block.produce ~state:state.protocol ~next_state_root_hash:None
      ~author:address ~operations:[] in
  let signature = Block.sign ~key:identity.secret block in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris ->
    let validator_uris = List.map snd validator_uris |> List.somes in
    let%await () =
      let open Network in
      broadcast_to_list
        (module Block_and_signature_spec)
        validator_uris { block; signature } in
    Format.printf "block.hash: %s\n%!" (BLAKE2B.to_string block.hash);
    Lwt.return (`Ok ())

let produce_block =
  let open Term in
  lwt_ret (const produce_block $ folder_node 0)

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
  (* FIXME: use the json output provided by Ulrik *)
  Format.printf "%s\n%!" (identity.t |> Key_hash.to_string);
  await (`Ok ())

let info_setup_identity =
  let doc = "Create a validator identity" in
  Cmd.info "setup-identity" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let setup_identity =
  let self_uri =
    let docv = "self_uri" in
    let doc = "The uri that other nodes should use to connect to this node." in
    let open Arg in
    required & opt (some uri) None & info ["uri"] ~doc ~docv in
  let open Term in
  lwt_ret (const setup_identity $ folder_node 0 $ self_uri)

let info_setup_tezos =
  let doc = "Setup Tezos identity" in
  Cmd.info "setup-tezos" ~version:"%%VERSION%%" ~doc ~exits ~man

let setup_tezos node_folder rpc_node secret consensus_contract
    discovery_contract required_confirmations =
  let%await () = ensure_folder node_folder in
  let%await () =
    write_interop_context ~node_folder
      {
        rpc_node;
        secret;
        consensus_contract;
        discovery_contract;
        required_confirmations;
      } in
  await (`Ok ())

let setup_tezos =
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
  let tezos_discovery_contract_address =
    let docv = "tezos_discovery_contract_address" in
    let doc = "The address of the Tezos discovery contract." in
    let open Arg in
    required
    & opt (some address_tezos_interop) None
    & info ["tezos_discovery_contract"] ~doc ~docv in
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
    $ folder_node 0
    $ tezos_node_uri
    $ tezos_secret
    $ tezos_consensus_contract_address
    $ tezos_discovery_contract_address
    $ tezos_required_confirmations)

let show_help =
  let doc = "a tool for interacting with the WIP Tezos Sidechain" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  ( (let open Term in
    ret (const (`Help (`Pager, None)))),
    Cmd.info "deku-cli" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits
      ~man )

let info_self =
  let doc = "Shows identity key and address of the node." in
  Cmd.info "self" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let self node_folder =
  let%await identity = read_identity ~node_folder in
  Format.printf "key: %s\n" (Wallet.to_string identity.key);
  Format.printf "address: %s\n" (Key_hash.to_string identity.t);
  Format.printf "uri: %s\n" (Uri.to_string identity.uri);
  await (`Ok ())

let self =
  let open Term in
  lwt_ret (const self $ folder_node 0)

let info_add_trusted_validator =
  let doc =
    "Helps node operators maintain a list of trusted validators they verified \
     off-chain which can later be used to make sure only trusted validators \
     are added as new validators in the network." in
  Cmd.info "add-trusted-validator" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

let add_trusted_validator node_folder address =
  let open Network in
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
    Network.request_trusted_validator_membership { signature; payload }
      identity.uri in
  await (`Ok ())

let validator_address =
  let docv = "validator_address" in
  let doc = "The validator address to be added/removed as trusted" in
  let open Arg in
  required & pos 1 (some address_implicit) None & info [] ~docv ~doc

let add_trusted_validator =
  let open Term in
  lwt_ret (const add_trusted_validator $ folder_node 0 $ validator_address)

let info_remove_trusted_validator =
  let doc =
    "Helps node operators maintain a list of trusted validators they verified \
     off-chain which can later be used to make sure only trusted validators \
     are added as new validators in the network." in
  Cmd.info "remove-trusted-validator" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

let remove_trusted_validator node_folder address =
  let open Network in
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
    Network.request_trusted_validator_membership { signature; payload }
      identity.uri in
  await (`Ok ())

let remove_trusted_validator =
  let open Term in
  lwt_ret (const remove_trusted_validator $ folder_node 0 $ validator_address)

let default_info =
  let doc = "Deku cli" in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "side-cli" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-cli"

let _ =
  Cmd.eval
  @@ Cmd.group default_info
       [
         Cmd.v info_create_wallet create_wallet;
         Cmd.v info_create_transaction create_transaction;
         Cmd.v info_create_custom_transaction create_custom_transaction;
         Cmd.v info_create_mock_transaction create_mock_transaction;
         Cmd.v info_originate_contract originate_contract;
         Cmd.v info_withdraw withdraw;
         Cmd.v info_withdraw_proof withdraw_proof;
         Cmd.v info_sign_block sign_block_term;
         Cmd.v info_produce_block produce_block;
         Cmd.v info_setup_identity setup_identity;
         Cmd.v info_setup_tezos setup_tezos;
         Cmd.v info_add_trusted_validator add_trusted_validator;
         Cmd.v info_remove_trusted_validator remove_trusted_validator;
         Cmd.v info_self self;
       ]
