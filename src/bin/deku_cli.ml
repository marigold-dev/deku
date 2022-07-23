open Helpers
open Crypto
open Protocol
open Cmdliner
open Core_deku
open Files

let () = Printexc.record_backtrace true

let read_identity ~node_folder =
  Config_files.Identity.read ~file:(node_folder ^ "/identity.json")

let man = [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]

let interop_context node_folder =
  let%await context =
    Config_files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
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
  let parser string = Ok string in
  let printer fmt arg = Format.fprintf fmt "%s" arg in
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

let contract_argument_addr =
  let parser string = Ok (Int64.of_string_opt string) in
  let printer fmt wallet =
    Format.fprintf fmt "%a"
      (Format.pp_print_option (fun fmt x ->
           Format.fprintf fmt "%s" (x |> Int64.to_string)))
      wallet in
  let open Arg in
  conv (parser, printer)

let contract_ticket_handle =
  let parser string =
    Int32.of_string_opt string
    |> Option.to_result ~none:(`Msg "Expected a valid Deku address.") in
  let printer fmt wallet = Format.fprintf fmt "%s" (wallet |> Int32.to_string) in
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

let ticket =
  let parser string =
    Core_deku.Ticket_id.of_string string
    |> Option.to_result ~none:(`Msg "Expected a ticket") in
  let printer fmt ticket =
    Format.fprintf fmt "%S" (Core_deku.Ticket_id.to_string ticket) in
  let open Arg in
  conv ~docv:"A ticket" (parser, printer)

let vm_flavor =
  let parser string =
    (match string with
    | "Wasm" -> Some `Wasm
    | _ -> None)
    |> Option.to_result ~none:(`Msg "Expected a valid Vm_flavor") in
  let printer fmt flavor =
    Format.fprintf fmt "%S"
      (match flavor with
      | `Wasm -> "Wasm") in
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
    Config_files.Wallet.write { priv_key = secret; address = key_hash } ~file
  in
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
  Cmd.info "create-transaction" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* Users should specify a paritcular node to send transactions, etc.
   to. We shouldn't fetch them dynamically here. TODO: replace this
   with a static setting defined either at CLI, in config file, or environment. *)
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
    ticket argument vm_flavor tickets =
  let open Network in
  with_validator_uri node_folder @@ fun (_, validator_uri) ->
  let%await block_level_response = request_block_level () validator_uri in
  let block_level = block_level_response.level in
  let%await wallet = Config_files.Wallet.read ~file:sender_wallet_file in
  let operation =
    match (Address.to_key_hash received_address, argument) with
    | Some addr, None ->
      Core_deku.User_operation.make ~source:wallet.address
        (Transaction { destination = addr; amount; ticket })
    | Some _, Some _ -> failwith "can't pass an argument to implicit account"
    | None, None -> failwith "Invalid transaction"
    | None, Some arg ->
      let payload =
        let open Contracts in
        match vm_flavor with
        | `Wasm ->
          let arg = Yojson.Safe.from_string arg in
          Contract_vm.Invocation_payload.wasm_of_yojson ~arg in
      let arg = payload |> Result.get_ok in
      Core_deku.User_operation.make ~source:wallet.address
        (Contract_invocation
           {
             to_invoke = Address.to_contract_hash received_address |> Option.get;
             argument = arg;
             tickets;
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

let info_originate_contract =
  let doc =
    "Originates a contract. Contract origination will be communicated to all \
     known validators to be included in the next block." in
  Cmd.info "originate-contract" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

let originate_contract node_folder contract_json initial_storage
    sender_wallet_file (vm_flavor : [`Wasm]) tickets =
  with_validator_uri node_folder (fun (_, validator_uri) ->
      let open Network in
      let%await block_level_response = request_block_level () validator_uri in
      let block_level = block_level_response.level in
      let%await wallet = Config_files.Wallet.read ~file:sender_wallet_file in
      let%await payload =
        let open Contracts in
        match vm_flavor with
        | `Wasm ->
          let%await code =
            Lwt_io.with_file ~mode:Input contract_json (fun x -> Lwt_io.read x)
          in
          let code = Bytes.of_string code in
          let initial_storage = Yojson.Safe.from_file initial_storage in
          let initial_storage =
            Yojson.Safe.Util.to_string initial_storage |> Bytes.of_string in
          Contract_vm.Origination_payload.wasm_of_yojson ~code
            ~storage:initial_storage
          |> Result.get_ok
          |> Lwt.return in
      let origination_op =
        User_operation.Contract_origination { payload; tickets } in
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
      Format.printf "contract_address: %s\noperation_hash: %s\n"
        (Contract_address.of_user_operation_hash originate_contract_op.hash
        |> Contract_address.to_string)
        (BLAKE2B.to_string originate_contract_op.hash);

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
  let vm_flavor =
    let doc = "Virtual machine flavor. can only be Wasm for now" in
    let env = Cmd.Env.info "VM_FLAVOR" ~doc in
    Arg.(
      Arg.value
      & opt ~vopt:`Wasm vm_flavor `Wasm
      & info ["vm_flavor"] ~env ~docv:"vm_flavor" ~doc) in
  let contract_json =
    let doc =
      "The path to the JSON output of compiling the contract to bytecode." in
    let open Arg in
    required
    & pos 2 (some contract_code_path) None
    & info [] ~docv:"contract" ~doc in
  let initial_storage =
    let doc = "The string containing initial storage for the VM" in
    let open Arg in
    required & pos 3 (some string) None & info [] ~docv:"initial_storage" ~doc
  in
  let tickets =
    let doc = "The string containing initial tickets for storage" in
    Arg.(
      Arg.value
      & opt ~vopt:[]
          (list
             (pair ~sep:'='
                (pair ~sep:':' ticket amount)
                (pair ~sep:':' contract_ticket_handle contract_argument_addr)))
          []
      & info ["tickets"] ~docv:"tickets" ~doc) in

  Term.(
    lwt_ret
      (const originate_contract
      $ folder_node 0
      $ contract_json
      $ initial_storage
      $ address_from
      $ vm_flavor
      $ tickets))

let create_transaction =
  let address_to =
    let doc = "The receiving address." in
    let env = Cmd.Env.info "RECEIVER" ~doc in
    let open Arg in
    required & pos 2 (some address) None & info [] ~env ~docv:"receiver" ~doc
  in
  let tickets =
    let doc = "The string containing tickets for vm argument" in
    Arg.(
      Arg.value
      & opt ~vopt:[]
          (list
             (pair ~sep:'='
                (pair ~sep:':' ticket amount)
                (pair ~sep:':' contract_ticket_handle contract_argument_addr)))
          []
      & info ["tickets"] ~docv:"tickets" ~doc) in
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
    let doc = "Virtual machine flavor. can be only be Wasm for now" in
    let env = Cmd.Env.info "VM_FLAVOR" ~doc in
    Arg.(
      Arg.value
      & opt ~vopt:`Wasm vm_flavor `Wasm
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
    $ vm_flavor
    $ tickets)

let info_withdraw =
  let doc = Printf.sprintf "Submits a withdraw to the sidechain." in
  Cmd.info "withdraw" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let withdraw node_folder sender_wallet_file tezos_address amount ticket =
  let open Network in
  let%await identity = read_identity ~node_folder in
  let%await block_level_response = request_block_level () identity.uri in
  let block_level = block_level_response.level in
  let%await wallet = Config_files.Wallet.read ~file:sender_wallet_file in
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

let info_get_ticket_balance =
  let doc = "Get balance of a ticket for an account." in
  Cmd.info "get-balance" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let info_create_counter_transaction =
  let doc = "Foo" in
  Cmd.info "create-counter-transaction" ~version:"%\226\128\140%VERSION%%" ~doc
    ~exits ~man

let get_ticket_balance node_folder address ticket =
  let open Network in
  let%await identity = read_identity ~node_folder in
  let%await result = request_ticket_balance { address; ticket } identity.uri in
  Format.printf "Balance: %i\n%!" (Amount.to_int result.amount);
  await (`Ok ())

let get_ticket_balance =
  let open Term in
  let folder_node =
    let docv = "folder_node" in
    let doc = "The folder where the node lives." in
    let open Arg in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let address =
    let docv = "address" in
    let doc = "The account address to get the balance." in
    let open Arg in
    required & pos 1 (some address_implicit) None & info [] ~docv ~doc in
  let ticket =
    let docv = "ticket" in
    let doc = "The ticket to get the balance." in
    let open Arg in
    required & pos 2 (some ticket) None & info [] ~docv ~doc in
  lwt_ret (const get_ticket_balance $ folder_node $ address $ ticket)

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
         Cmd.v info_originate_contract originate_contract;
         Cmd.v info_withdraw withdraw;
         Cmd.v info_withdraw_proof withdraw_proof;
         Cmd.v info_get_ticket_balance get_ticket_balance;
         Cmd.v info_self self;
       ]
