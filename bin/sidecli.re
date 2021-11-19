open Helpers;
open Crypto;
open Node;
open State;
open Protocol;
open Cmdliner;

Printexc.record_backtrace(true);

// file helpers

let read_validators = (~node_folder) =>
  Files.Validators.read(~file=node_folder ++ "/validators.json");
let read_identity = (~node_folder) =>
  Files.Identity.read(~file=node_folder ++ "/identity.json");
let write_identity = (~node_folder) =>
  Files.Identity.write(~file=node_folder ++ "/identity.json");
let write_interop_context = (~node_folder) =>
  Files.Interop_context.write(~file=node_folder ++ "/tezos.json");
let read_state = (~node_folder): Lwt.t(Protocol.t) =>
  Lwt_io.with_file(
    ~mode=Input,
    node_folder ++ "/state.bin",
    Lwt_io.read_value,
  );

let man = [
  `S(Manpage.s_bugs),
  `P("Email bug reports to <contact@marigold.dev>."),
];

// Todo from Andre: we may have to do peer discovery?
// I don't know anything about that, except for having played with hyperswarm.
let validators_uris = node_folder => {
  let.await validators = read_validators(~node_folder);
  validators |> List.map(snd) |> await;
};

let make_filename_from_address = wallet_addr_str => {
  Printf.sprintf("%s.tzsidewallet", wallet_addr_str);
};

let exits =
  Term.default_exits
  @ Term.[exit_info(1, ~doc="expected failure (might not be a bug)")];

let lwt_ret = p => Term.(ret(const(Lwt_main.run) $ p));

// Arguments
// ==========

// TODO: Wallet.t
let wallet = {
  let parser = file => {
    let non_dir_file = Arg.(conv_parser(non_dir_file));
    switch (
      non_dir_file(file),
      non_dir_file(make_filename_from_address(file)),
    ) {
    | (Ok(file), _)
    | (_, Ok(file)) => Ok(file)
    | _ => Error(`Msg("Expected path to wallet"))
    };
  };
  let printer = Arg.(conv_printer(non_dir_file));
  Arg.(conv((parser, printer)));
};

let edsk_secret_key = {
  let parser = key => {
    switch (Crypto.Secret.of_string(key)) {
    | Some(key) => Ok(key)
    | _ => Error(`Msg("Expected EDSK secret key"))
    };
  };
  let printer = (ppf, key) => {
    Format.fprintf(ppf, "%s", Crypto.Secret.to_string(key));
  };
  Arg.(conv((parser, printer)));
};

let uri = {
  // TODO: check that uri is valid
  let parser = uri => Ok(uri |> Uri.of_string);
  let printer = (ppf, uri) => {
    Format.fprintf(ppf, "%s", uri |> Uri.to_string);
  };
  Arg.(conv((parser, printer)));
};

let address = {
  let parser = string =>
    Key_hash.of_string(string)
    |> Option.map(Address.of_key_hash)
    |> Option.to_result(~none=`Msg("Expected a wallet address."));
  let printer = (fmt, wallet) =>
    Format.fprintf(
      fmt,
      "%s",
      wallet |> Address.to_key_hash |> Key_hash.to_string,
    );
  Arg.(conv((parser, printer)));
};

let address_tezos_interop = {
  let parser = string =>
    string
    |> Tezos.Address.of_string
    |> Option.to_result(~none=`Msg("Expected a wallet address."));
  let printer = (fmt, address) =>
    Format.fprintf(fmt, "%s", Tezos.Address.to_string(address));
  Arg.(conv((parser, printer)));
};
let amount = {
  let parser = string => {
    let.ok int =
      int_of_string_opt(string)
      |> Option.to_result(~none=`Msg("Expected an amount"));
    // TODO: probably of_int should be option
    try(Ok(Amount.of_int(int))) {
    | _exn => Error(`Msg("Expected an amount above zero"))
    };
  };
  let printer = (fmt, amount) =>
    Format.fprintf(fmt, "%d", Amount.to_int(amount));
  Arg.(conv(~docv="A positive amount", (parser, printer)));
};

let tezos_required_confirmations = {
  let msg = "Expected an integer greater than 0";
  let parser = string =>
    switch (int_of_string_opt(string)) {
    | Some(int) when int > 0 => Ok(int)
    | Some(_)
    | None => Error(`Msg(msg))
    };
  let printer = (fmt, int) => Format.fprintf(fmt, "%d", int);
  Arg.(conv(~docv="An integer greater than 0", (parser, printer)));
};

let ticket = {
  let parser = string =>
    Tezos.Ticket_id.of_string(string)
    |> Option.to_result(~none=`Msg("Expected a ticket"));
  let printer = (fmt, ticket) =>
    Format.fprintf(fmt, "%S", Tezos.Ticket_id.to_string(ticket));
  Arg.(conv(~docv="A ticket", (parser, printer)));
};
let hash = {
  let parser = string =>
    BLAKE2B.of_string(string)
    |> Option.to_result(~none=`Msg("Expected a 256bits BLAKE2b hash."));
  let printer = (fmt, wallet) =>
    Format.fprintf(fmt, "%s", BLAKE2B.to_string(wallet));
  Arg.(conv((parser, printer)));
};

// Commands
// ========

// create-wallet

let info_create_wallet = {
  let doc = "Creates a wallet file. The wallet file's filename is its address. The wallet file contains the private uri corresponding to that address.";
  Term.info("create-wallet", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let create_wallet = () => {
  let (key, address) = Address.make();

  let address_string = Address.to_string(address);
  let file = make_filename_from_address(address_string);

  let.await () = Files.Wallet.write({priv_key: key, address}, ~file);
  await(`Ok());
};

let create_wallet = Term.(lwt_ret(const(create_wallet) $ const()));

// create-transaction

let info_create_transaction = {
  let doc =
    Printf.sprintf(
      "Submits a transaction to the sidechain. The transaction will be communicated to all known validators to be included in the next block. If the path to the wallet file corresponding to the sending address is not provided, a wallet file with the correct filename (%s) must be present in the current working directory",
      make_filename_from_address("address"),
    );
  Term.info(
    "create-transaction",
    ~version="%‌%VERSION%%",
    ~doc,
    ~exits,
    ~man,
  );
};

let create_transaction =
    (node_folder, sender_wallet_file, received_address, amount, ticket) => {
  open Networking;
  let.await validators_uris = validators_uris(node_folder);
  let validator_uri = List.hd(validators_uris);
  let.await block_level_response = request_block_level((), validator_uri);
  let block_level = block_level_response.level;
  let.await wallet = Files.Wallet.read(~file=sender_wallet_file);
  let transaction =
    Operation.Side_chain.sign(
      ~secret=wallet.priv_key,
      ~nonce=0l,
      ~block_height=block_level,
      ~source=wallet.address,
      ~kind=Transaction({destination: received_address, amount, ticket}),
    );
  let.await identity = read_identity(~node_folder);

  // Broadcast transaction
  let.await () =
    Networking.request_operation_gossip(
      Networking.Operation_gossip.{operation: transaction},
      identity.uri,
    );
  Format.printf(
    "operation.hash: %s\n%!",
    BLAKE2B.to_string(transaction.hash),
  );

  Lwt.return(`Ok());
};

let folder_node = {
  let docv = "folder_node";
  let doc = "The folder where the node lives.";
  Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
};

let create_transaction = {
  let address_from = {
    let doc = "The sending address, or a path to a wallet. If a bare sending address is provided, the corresponding wallet is assumed to be in the working directory.";
    let env = Arg.env_var("SENDER", ~doc);
    Arg.(
      required
      & pos(1, some(wallet), None)
      & info([], ~env, ~docv="sender", ~doc)
    );
  };

  let address_to = {
    let doc = "The receiving address.";
    let env = Arg.env_var("RECEIVER", ~doc);
    Arg.(
      required
      & pos(2, some(address), None)
      & info([], ~env, ~docv="receiver", ~doc)
    );
  };

  let amount = {
    let doc = "The amount to be transferred.";
    let env = Arg.env_var("TRANSFER_AMOUNT", ~doc);
    Arg.(
      required
      & pos(3, some(amount), None)
      & info([], ~env, ~docv="amount", ~doc)
    );
  };

  let ticket = {
    let doc = "The ticket to be transferred.";
    Arg.(
      required & pos(4, some(ticket), None) & info([], ~docv="MSG", ~doc)
    );
  };

  Term.(
    lwt_ret(
      const(create_transaction)
      $ folder_node
      $ address_from
      $ address_to
      $ amount
      $ ticket,
    )
  );
};

// withdraw

let info_withdraw = {
  let doc = Printf.sprintf("Submits a withdraw to the sidechain.");
  Term.info("withdraw", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let withdraw =
    (node_folder, sender_wallet_file, tezos_address, amount, ticket) => {
  open Networking;
  let.await identity = read_identity(~node_folder);
  let.await block_level_response = request_block_level((), identity.uri);
  let block_level = block_level_response.level;
  let.await wallet = Files.Wallet.read(~file=sender_wallet_file);
  let operation =
    Operation.Side_chain.sign(
      ~secret=wallet.priv_key,
      ~nonce=0l,
      ~block_height=block_level,
      ~source=wallet.address,
      ~kind=Withdraw({owner: tezos_address, amount, ticket}),
    );

  // Broadcast transaction
  let.await () =
    Networking.request_operation_gossip(
      Networking.Operation_gossip.{operation: operation},
      identity.uri,
    );

  Format.printf("operation.hash: %s\n%!", BLAKE2B.to_string(operation.hash));
  Lwt.return(`Ok());
};

let withdraw = {
  let folder_node = {
    let docv = "folder_node";
    let doc = "The folder where the node lives.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  let address_from = {
    let doc = "The sending address, or a path to a wallet. If a bare sending address is provided, the corresponding wallet is assumed to be in the working directory.";
    let env = Arg.env_var("SENDER", ~doc);
    Arg.(
      required
      & pos(1, some(wallet), None)
      & info([], ~env, ~docv="sender", ~doc)
    );
  };

  let tezos_address = {
    let doc = "The address that will be used to withdraw the ticket at Tezos **only KT1 and tz1**";
    Arg.(
      required
      & pos(2, some(address_tezos_interop), None)
      & info([], ~docv="tezos_address", ~doc)
    );
  };

  let amount = {
    let doc = "The amount to be transacted.";
    let env = Arg.env_var("TRANSFER_AMOUNT", ~doc);
    Arg.(
      required
      & pos(3, some(amount), None)
      & info([], ~env, ~docv="amount", ~doc)
    );
  };

  let ticket = {
    let doc = "The ticket to be trasnsacted.";
    Arg.(
      required & pos(4, some(ticket), None) & info([], ~docv="MSG", ~doc)
    );
  };

  Term.(
    lwt_ret(
      const(withdraw)
      $ folder_node
      $ address_from
      $ tezos_address
      $ amount
      $ ticket,
    )
  );
};

// withdraw-proof

let withdraw_proof = (node_folder, operation_hash, callback) => {
  open Networking;

  let.await identity = read_identity(~node_folder);

  let.await result =
    request_withdraw_proof({operation_hash: operation_hash}, identity.uri);
  switch (result) {
  | Unknown_operation =>
    let message = BLAKE2B.to_string(operation_hash) ++ " is unknown";
    await(`Error((false, message)));
  | Operation_is_not_a_withdraw =>
    let message = BLAKE2B.to_string(operation_hash) ++ " is not a withdraw";
    await(`Error((false, message)));
  | Ok({handles_hash, handle, proof}) =>
    let to_hex = bytes => Hex.show(Hex.of_bytes(bytes));
    Format.printf(
      {|(Pair (Pair %S
            (Pair (Pair %d 0x%s) %d %S)
            %S)
      0x%s
      { %s })|},
      Tezos.Address.to_string(callback),
      Amount.to_int(handle.amount),
      to_hex(handle.ticket.data),
      handle.id,
      Tezos.Address.to_string(handle.owner),
      Tezos.Address.to_string(handle.ticket.ticketer),
      BLAKE2B.to_string(handles_hash),
      List.map(
        ((left, right)) =>
          Format.sprintf(
            "        Pair 0x%s\n             0x%s",
            BLAKE2B.to_string(left),
            BLAKE2B.to_string(right),
          ),
        proof,
      )
      |> String.concat(" ;\n")
      |> String.trim,
    );
    await(`Ok());
  };
};
let info_withdraw_proof = {
  let doc = "Find withdraw proof from operation hash";
  Term.info("withdraw-proof", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};
let withdraw_proof = {
  let folder_dest = {
    let docv = "folder_dest";
    let doc = "The folder the files will be created in. The folder must exist and be empty.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  let operation_hash = {
    let docv = "operation_hash";
    let doc = "The operation hash used on the withdraw.";
    Arg.(required & pos(1, some(hash), None) & info([], ~doc, ~docv));
  };

  let contract_callback = {
    let docv = "contract_callback";
    let doc = "Contract callback to be used on the withdraw";
    Arg.(
      required
      & pos(2, some(address_tezos_interop), None)
      & info([], ~doc, ~docv)
    );
  };

  Term.(
    lwt_ret(
      const(withdraw_proof) $ folder_dest $ operation_hash $ contract_callback,
    )
  );
};

// sign-block

let info_sign_block = {
  let doc = "Sign a block hash and broadcast to the network manually, useful when the chain is stale.";
  Term.info("sign-block", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};
let sign_block = (node_folder, block_hash) => {
  let.await identity = read_identity(~node_folder);
  let signature = Signature.sign(~key=identity.secret, block_hash);
  let.await validators_uris = validators_uris(node_folder);
  let.await () =
    Networking.(
      broadcast_to_list(
        (module Signature_spec),
        validators_uris,
        {hash: block_hash, signature},
      )
    );
  Lwt.return(`Ok());
};
let sign_block_term = {
  let folder_node = {
    let docv = "folder_node";
    let doc = "The folder where the node lives.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  let block_hash = {
    let doc = "The block hash to be signed.";
    Arg.(required & pos(1, some(hash), None) & info([], ~doc));
  };

  Term.(lwt_ret(const(sign_block) $ folder_node $ block_hash));
};

// produce-block
let info_produce_block = {
  let doc = "Produce and sign a block and broadcast to the network manually, useful when the chain is stale.";
  Term.info("produce-block", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let produce_block = node_folder => {
  let.await identity = read_identity(~node_folder);
  let.await state = read_state(~node_folder);
  let address = identity.t;
  let block =
    Block.produce(
      ~state,
      ~author=address,
      ~main_chain_ops=[],
      ~side_chain_ops=[],
    );
  let signature = Block.sign(~key=identity.secret, block);
  let.await validators_uris = validators_uris(node_folder);
  let.await () =
    Networking.(
      broadcast_to_list(
        (module Block_and_signature_spec),
        validators_uris,
        {block, signature},
      )
    );
  Format.printf("block.hash: %s\n%!", BLAKE2B.to_string(block.hash));
  Lwt.return(`Ok());
};

let produce_block = {
  let folder_node = {
    let docv = "folder_node";
    let doc = "The folder where the node lives.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  Term.(lwt_ret(const(produce_block) $ folder_node));
};

let ensure_folder = folder => {
  let.await exists = Lwt_unix.file_exists(folder);
  if (exists) {
    let.await stat = Lwt_unix.stat(folder);
    // TODO: check permissions
    if (stat.st_kind == Lwt_unix.S_DIR) {
      await();
    } else {
      raise(Invalid_argument(folder ++ " is not a folder"));
    };
  } else {
    Lwt_unix.mkdir(folder, 0o700);
  };
};
let setup_identity = (node_folder, uri) => {
  let.await () = ensure_folder(node_folder);

  let identity = {
    let (secret, key) = Crypto.Ed25519.generate();
    let t = Address.of_wallet(Ed25519(key));
    {uri, t, key: Ed25519(key), secret: Ed25519(secret)};
  };
  let.await () = write_identity(~node_folder, identity);
  await(`Ok());
};
let info_setup_identity = {
  let doc = "Create a validator identity";
  Term.info("setup-identity", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};
let setup_identity = {
  let folder_dest = {
    let docv = "folder_dest";
    let doc = "The folder the files will be created in. The folder must exist and be empty.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  let self_uri = {
    let docv = "self_uri";
    let doc = "The uri that other nodes should use to connect to this node.";
    Arg.(required & opt(some(uri), None) & info(["uri"], ~doc, ~docv));
  };

  Term.(lwt_ret(const(setup_identity) $ folder_dest $ self_uri));
};

let info_setup_tezos = {
  let doc = "Setup Tezos identity";
  Term.info("setup-tezos", ~version="%%VERSION%%", ~doc, ~exits, ~man);
};
let setup_tezos =
    (
      node_folder,
      rpc_node,
      secret,
      consensus_contract,
      required_confirmations,
    ) => {
  let.await () = ensure_folder(node_folder);

  let context =
    Tezos_interop.Context.{
      rpc_node,
      secret,
      consensus_contract,
      required_confirmations,
    };
  let.await () = write_interop_context(~node_folder, context);

  await(`Ok());
};
let setup_tezos = {
  let folder_dest = {
    let docv = "folder_dest";
    let doc = "The folder the files will be created in. The folder must exist and be empty.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  let tezos_node_uri = {
    let docv = "tezos_node_uri";
    let doc = "The uri of the tezos node.";
    Arg.(
      required
      & opt(some(uri), None)
      & info(["tezos_rpc_node"], ~doc, ~docv)
    );
  };

  let tezos_secret = {
    let docv = "tezos_secret";
    let doc = "The Tezos secret key.";
    Arg.(
      required
      & opt(some(edsk_secret_key), None)
      & info(["tezos_secret"], ~doc, ~docv)
    );
  };

  let tezos_consensus_contract_address = {
    let docv = "tezos_consensus_contract_address";
    let doc = "The address of the Tezos consensus contract.";
    Arg.(
      required
      & opt(some(address_tezos_interop), None)
      & info(["tezos_consensus_contract"], ~doc, ~docv)
    );
  };

  let tezos_required_confirmations = {
    let docv = "int";
    let doc = "Set the required confirmations. WARNING: Setting below default of 10 can compromise security of the Deku chain.";
    Arg.(
      value
      & opt(tezos_required_confirmations, 10)
      & info(["unsafe_tezos_required_confirmations"], ~doc, ~docv)
    );
  };

  Term.(
    lwt_ret(
      const(setup_tezos)
      $ folder_dest
      $ tezos_node_uri
      $ tezos_secret
      $ tezos_consensus_contract_address
      $ tezos_required_confirmations,
    )
  );
};

// Term that just shows the help command, to use when no arguments are passed
let show_help = {
  let doc = "a tool for interacting with the WIP Tezos Sidechain";
  let sdocs = Manpage.s_common_options;
  let exits = Term.default_exits;
  (
    Term.(ret(const(`Help((`Pager, None))))),
    Term.info("sidecli", ~version="v0.0.1", ~doc, ~sdocs, ~exits, ~man),
  );
};

let info_self = {
  let doc = "Shows identity key and address of the node.";
  Term.info("self", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let self = node_folder => {
  let.await identity = read_identity(~node_folder);
  Format.printf("key: %s\n", Wallet.to_string(identity.key));
  Format.printf("address: %s\n", Address.to_string(identity.t));
  Format.printf("uri: %s\n", Uri.to_string(identity.uri));
  await(`Ok());
};

let self = {
  let folder_dest = {
    let docv = "folder_dest";
    let doc = "The folder of the node.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  Term.(lwt_ret(const(self) $ folder_dest));
};

let info_add_trusted_validator = {
  let doc = "Helps node operators maintain a list of trusted validators they verified off-chain which can later be used to make sure only trusted validators are added as new validators in the network.";
  Term.info(
    "add-trusted-validator",
    ~version="%‌%VERSION%%",
    ~doc,
    ~exits,
    ~man,
  );
};

let add_trusted_validator = (node_folder, address) => {
  open Networking;
  let.await identity = read_identity(~node_folder);
  let payload = Trusted_validators_membership_change.{address, action: Add};
  let payload_json_str =
    payload
    |> Trusted_validators_membership_change.payload_to_yojson
    |> Yojson.Safe.to_string;
  let payload_hash = BLAKE2B.hash(payload_json_str);
  let signature = Signature.sign(~key=identity.secret, payload_hash);
  let.await () =
    Networking.request_trusted_validator_membership(
      {signature, payload},
      identity.uri,
    );
  await(`Ok());
};

let address_t = {
  let parser = string =>
    string
    |> Address.of_string
    |> Option.to_result(~none=`Msg("Expected a validator address."));
  let printer = (fmt, address) =>
    Format.fprintf(fmt, "%s", Address.to_string(address));
  Arg.(conv((parser, printer)));
};

let validator_address = {
  let docv = "validator_address";
  let doc = "The validator address to be added/removed as trusted";
  Arg.(required & pos(1, some(address_t), None) & info([], ~docv, ~doc));
};

let add_trusted_validator = {
  Term.(
    lwt_ret(const(add_trusted_validator) $ folder_node $ validator_address)
  );
};

let info_remove_trusted_validator = {
  let doc = "Helps node operators maintain a list of trusted validators they verified off-chain which can later be used to make sure only trusted validators are added as new validators in the network.";
  Term.info(
    "remove-trusted-validator",
    ~version="%‌%VERSION%%",
    ~doc,
    ~exits,
    ~man,
  );
};

let remove_trusted_validator = (node_folder, address) => {
  open Networking;
  let.await identity = read_identity(~node_folder);
  let payload =
    Trusted_validators_membership_change.{address, action: Remove};
  let payload_json_str =
    payload
    |> Trusted_validators_membership_change.payload_to_yojson
    |> Yojson.Safe.to_string;
  let payload_hash = BLAKE2B.hash(payload_json_str);
  let signature = Signature.sign(~key=identity.secret, payload_hash);
  let.await () =
    Networking.request_trusted_validator_membership(
      {signature, payload},
      identity.uri,
    );
  await(`Ok());
};

let remove_trusted_validator = {
  Term.(
    lwt_ret(
      const(remove_trusted_validator) $ folder_node $ validator_address,
    )
  );
};

// Run the CLI

let () = {
  Term.exit @@
  Term.eval_choice(
    show_help,
    [
      (create_wallet, info_create_wallet),
      (create_transaction, info_create_transaction),
      (withdraw, info_withdraw),
      (withdraw_proof, info_withdraw_proof),
      (sign_block_term, info_sign_block),
      (produce_block, info_produce_block),
      (setup_identity, info_setup_identity),
      (setup_tezos, info_setup_tezos),
      (add_trusted_validator, info_add_trusted_validator),
      (remove_trusted_validator, info_remove_trusted_validator),
      (self, info_self),
    ],
  );
};
