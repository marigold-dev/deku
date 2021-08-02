open Helpers;
open Node;
open State;
open Protocol;
open Cmdliner;

Printexc.record_backtrace(true);

let man = [
  `S(Manpage.s_bugs),
  `P("Email bug reports to <contact@marigold.dev>."),
];

// Todo from Andre: we may have to do peer discovery?
// I don't know anything about that, except for having played with hyperswarm.
let validators = () =>
  [
    Files.Identity.read(~file="0/identity.json"),
    Files.Identity.read(~file="1/identity.json"),
    Files.Identity.read(~file="2/identity.json"),
    Files.Identity.read(~file="3/identity.json"),
  ]
  |> Lwt.all;

let validators_uris = () =>
  Lwt.map(List.map(validator => validator.uri), validators());

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
    switch (Tezos_interop.Secret.of_string(key)) {
    | Some(key) => Ok(key)
    | _ => Error(`Msg("Expected EDSK secret key"))
    };
  };
  let printer = (ppf, key) => {
    Format.fprintf(ppf, "%s", Tezos_interop.Secret.to_string(key));
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
    BLAKE2B_20.of_string(string)
    |> Option.map(Wallet.address_of_blake)
    |> Option.to_result(~none=`Msg("Expected a wallet address."));
  let printer = (fmt, wallet) =>
    Format.fprintf(fmt, "%s", Wallet.address_to_string(wallet));
  Arg.(conv((parser, printer)));
};
let address_tezos_interop = {
  let parser = string =>
    string
    |> Tezos_interop.Address.of_string
    |> Option.to_result(~none=`Msg("Expected a wallet address."));
  let printer = (fmt, address) =>
    Format.fprintf(fmt, "%s", Tezos_interop.Address.to_string(address));
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
let ticket = {
  let parser = string =>
    Tezos_interop.Ticket.of_string(string)
    |> Option.to_result(~none=`Msg("Expected a ticket"));
  let printer = (fmt, ticket) =>
    Format.fprintf(fmt, "%S", Tezos_interop.Ticket.to_string(ticket));
  Arg.(conv(~docv="A ticket", (parser, printer)));
};

// Commands
// ========

// create-wallet

let info_create_wallet = {
  let doc = "Creates a wallet file. The wallet file's filename is its address. The wallet file contains the private uri corresponding to that address.";
  Term.info("create-wallet", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let create_wallet = () => {
  let (key, wallet) = Wallet.make_wallet();

  let wallet_addr_str = Wallet.address_to_string(wallet);
  let file = make_filename_from_address(wallet_addr_str);

  let.await () = Files.Wallet.write({priv_key: key, address: wallet}, ~file);
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
    (sender_wallet_file, received_address, amount, ticket) => {
  open Networking;
  let.await validators_uris = validators_uris();
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
      ~amount,
      ~ticket,
      ~kind=Transaction({destination: received_address}),
    );

  // Broadcast transaction
  let.await () =
    Networking.broadcast_operation_gossip_to_list(
      validators_uris,
      Networking.Operation_gossip.{operation: transaction},
    );

  Lwt.return(`Ok());
};

let create_transaction = {
  let address_from = {
    let doc = "The sending address, or a path to a wallet. If a bare sending address is provided, the corresponding wallet is assumed to be in the working directory.";
    let env = Arg.env_var("SENDER", ~doc);
    Arg.(
      required
      & pos(0, some(wallet), None)
      & info([], ~env, ~docv="sender", ~doc)
    );
  };

  let address_to = {
    let doc = "The receiving address.";
    let env = Arg.env_var("RECEIVER", ~doc);
    Arg.(
      required
      & pos(1, some(address), None)
      & info([], ~env, ~docv="receiver", ~doc)
    );
  };

  let amount = {
    let doc = "The amount to be transacted.";
    let env = Arg.env_var("TRANSFER_AMOUNT", ~doc);
    Arg.(
      required
      & pos(2, some(amount), None)
      & info([], ~env, ~docv="amount", ~doc)
    );
  };

  let ticket = {
    let doc = "The ticket to be trasnsacted.";
    Arg.(
      required & pos(3, some(ticket), None) & info([], ~docv="MSG", ~doc)
    );
  };

  Term.(
    lwt_ret(
      const(create_transaction) $ address_from $ address_to $ amount $ ticket,
    )
  );
};

// sign-block
let hash = {
  let parser = string =>
    BLAKE2B.of_string(string)
    |> Option.to_result(~none=`Msg("Expected a 256bits BLAKE2b hash."));
  let printer = (fmt, wallet) =>
    Format.fprintf(fmt, "%s", BLAKE2B.to_string(wallet));
  Arg.(conv((parser, printer)));
};

let info_sign_block = {
  let doc = "Sign a block hash and broadcast to the network manually, useful when the chain is stale.";
  Term.info("sign-block", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};
let sign_block = (wallet_file, block_hash) => {
  let.await wallet = Files.Wallet.read(~file=wallet_file);
  let signature = Signature.sign(~key=wallet.priv_key, block_hash);
  let.await validators_uris = validators_uris();
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
  let key_wallet = {
    let doc = "The validator key that will sign the block address.";
    Arg.(required & pos(0, some(wallet), None) & info([], ~doc));
  };

  let block_hash = {
    let doc = "The block hash to be signed.";
    Arg.(required & pos(1, some(hash), None) & info([], ~doc));
  };

  Term.(lwt_ret(const(sign_block) $ key_wallet $ block_hash));
};

// produce-block
let info_produce_block = {
  let doc = "Produce and sign a block and broadcast to the network manually, useful when the chain is stale.";
  Term.info("produce-block", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let produce_block = (wallet_file, state_bin) => {
  let.await wallet = Files.Wallet.read(~file=wallet_file);
  let.await state: Protocol.t =
    Lwt_io.with_file(~mode=Input, state_bin, Lwt_io.read_value);
  let address = Address.of_key(wallet.priv_key);
  let block =
    Block.produce(
      ~state,
      ~author=address,
      ~main_chain_ops=[],
      ~side_chain_ops=[],
    );
  let signature = Block.sign(~key=wallet.priv_key, block);
  let.await validators_uris = validators_uris();
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
  let key_wallet = {
    let docv = "key_wallet";
    let doc = "The validator key that will sign the block address.";
    Arg.(required & pos(0, some(wallet), None) & info([], ~doc, ~docv));
  };

  let state_bin = {
    let docv = "state_bin";
    let doc = "Path to last known serialized state.";
    Arg.(
      required & pos(1, some(non_dir_file), None) & info([], ~doc, ~docv)
    );
  };

  Term.(lwt_ret(const(produce_block) $ key_wallet $ state_bin));
};

// gen credentials
let info_gen_credentials = {
  let doc = "Generate initial set of validator credentials. Note: Doesn't create a wallet. See create-wallet for more info.";
  Term.info(
    "make-credentials",
    ~version="%‌%VERSION%%",
    ~doc,
    ~exits,
    ~man,
  );
};
let gen_credentials = {
  let make_identity_file = (file, index) => {
    open Mirage_crypto_ec;
    let.await () = Lwt_unix.mkdir(Printf.sprintf("./%d/", index), 0o700);
    let file = Printf.sprintf("./%d/%s", index, file);
    let uri = Printf.sprintf("http://localhost:%d", 4440 + index);

    let uri = Uri.of_string(uri);
    let (key, t) = Ed25519.generate();
    let identity = {key, t, uri};
    let.await () = Files.Identity.write(identity, ~file);
    await((t, uri));
  };

  let make_validators_files = (file, validators, to_make) => {
    module T = {
      [@deriving to_yojson]
      type t = {
        address: Address.t,
        uri: Uri.t,
      };
    };

    Lwt_list.iter_p(
      i =>
        Files.Validators.write(
          validators,
          ~file=Printf.sprintf("./%d/%s", i, file),
        ),
      to_make,
    );
  };

  let to_make = [0, 1, 2, 3];

  Term.(
    lwt_ret(
      const(() => {
        let.await validators =
          Lwt_list.map_p(make_identity_file("identity.json"), to_make);
        let.await () =
          make_validators_files("validators.json", validators, to_make);
        await(`Ok());
      })
      $ const(),
    )
  );
};

// inject genesis block
let info_inject_genesis = {
  let doc = "Injects the genesis block";
  Term.info("inject-genesis", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let inject_genesis = {
  let make_new_block = validators => {
    let first = List.hd(validators);
    let state = Protocol.make(~initial_block=Block.genesis);
    let.await state = {
      let.await validators = Files.Validators.read(~file="0/validators.json");
      Lwt.return({
        ...state,
        validators:
          List.fold_right(
            ((address, _)) => Validators.add({address: address}),
            validators,
            Validators.empty,
          ),
      });
    };
    let block =
      Block.produce(
        ~state,
        ~author=first.t,
        ~main_chain_ops=[],
        ~side_chain_ops=[],
      );
    Printf.printf(
      "block_hash: %s, state_hash: %s, block_height: %Ld, validators: %s%!",
      BLAKE2B.to_string(block.hash),
      BLAKE2B.to_string(block.state_root_hash),
      block.block_height,
      state.validators
      |> Validators.to_list
      |> List.map(validator =>
           Tezos_interop.Key.Ed25519(validator.Validators.address)
         )
      |> List.map(Tezos_interop.Key.to_string)
      |> String.concat(","),
    );

    let signature = Block.sign(~key=first.key, block);
    let.await validators_uris = validators_uris();
    let.await () =
      Networking.(
        broadcast_to_list(
          (module Block_and_signature_spec),
          validators_uris,
          {block, signature},
        )
      );
    Lwt.return();
  };

  Term.(
    lwt_ret(
      const(() => {
        let.await validators = validators();
        let.await () = make_new_block(validators);
        Lwt.return(`Ok());
      })
      $ const(),
    )
  );
};

// Create files needed for the node's operation
let info_setup_node = {
  let doc = "Creates the files needed to setup a node.";
  Term.info("setup-node", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let setup_node =
    (
      folder,
      secret,
      uri,
      tezos_rpc_node,
      tezos_secret,
      tezos_consensus_contract,
    ) => {
  let in_folder = Filename.concat(folder);

  let secret = {
    switch (secret) {
    | Tezos_interop.Secret.Ed25519(secret) => secret
    };
  };
  let address = Address.of_key(secret);
  let (identity, identity_path) = (
    State.{key: secret, uri, t: address},
    "identity.json" |> in_folder,
  );

  let (wallet, wallet_path) = (
    Files.Wallet.{address: Wallet.of_address(address), priv_key: secret},
    "wallet.json" |> in_folder,
  );

  let (validators, validators_path) = ([], "validators.json" |> in_folder); // TODO: Populate validators
  let (interop_context, interop_context_path) = (
    Tezos_interop.Context.{
      rpc_node: tezos_rpc_node,
      secret: tezos_secret,
      consensus_contract: tezos_consensus_contract,
      required_confirmations: 10,
    },
    "interop_context.json" |> in_folder,
  );

  let create_files = () => {
    let.await () = Files.Identity.write(identity, ~file=identity_path);
    let.await () = Files.Validators.write(validators, ~file=validators_path);
    let.await () = Files.Wallet.write(wallet, ~file=wallet_path);
    let.await () =
      Files.Interop_context.write(
        interop_context,
        ~file=interop_context_path,
      );
    Lwt.return(`Ok());
  };

  if (Sys.file_exists(folder)) {
    if (Sys.is_directory(folder)) {
      if (Sys.readdir(folder) == [||]
          || Sys.readdir(folder) == [|".gitkeep"|]) {
        create_files();
      } else {
        Lwt.return(`Error((false, folder ++ " is not empty.")));
      };
    } else {
      Lwt.return(`Error((false, folder ++ " is not a directory.")));
    };
  } else {
    let.await () = Lwt_unix.mkdir(folder, 0o700);
    create_files();
  };
};

let setup_node = {
  let folder_dest = {
    let docv = "folder_dest";
    let doc = "The folder the files will be created in. The folder must exist and be empty.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  // TODO: figure out how to make the rest of these required named arguments so people don't get the order confused.
  let secret = {
    let docv = "secret";
    let doc = "The secret key used by the validator.";
    Arg.(
      required
      & opt(some(edsk_secret_key), None)
      & info(["secret"], ~doc, ~docv)
    );
  };

  let self_uri = {
    let docv = "self_uri";
    let doc = "The uri that other nodes should use to connect to this node.";
    Arg.(required & opt(some(uri), None) & info(["uri"], ~doc, ~docv));
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

  Term.(
    lwt_ret(
      const(setup_node)
      $ folder_dest
      $ secret
      $ self_uri
      $ tezos_node_uri
      $ tezos_secret
      $ tezos_consensus_contract_address,
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

// Run the CLI

let () = {
  Mirage_crypto_rng_unix.initialize();
  Term.exit @@
  Term.eval_choice(
    show_help,
    [
      (create_wallet, info_create_wallet),
      (create_transaction, info_create_transaction),
      (sign_block_term, info_sign_block),
      (produce_block, info_produce_block),
      (gen_credentials, info_gen_credentials),
      (inject_genesis, info_inject_genesis),
      (setup_node, info_setup_node),
    ],
  );
};
