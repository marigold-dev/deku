open Helpers;
open Node;
open State;
open Protocol;
open Cmdliner;

Printexc.record_backtrace(true);

// Helpers

let read_file = file => {
  let.await lines =
    Lwt_io.with_file(~mode=Input, file, ic =>
      Lwt_io.read_lines(ic) |> Lwt_stream.to_list
    );
  await(lines |> String.concat("\n"));
};

let read_identity_file = file => {
  let.await file_buffer = read_file(file);
  let json = Yojson.Safe.from_string(file_buffer);
  identity_of_yojson(json) |> Result.get_ok |> Lwt.return;
};

let write_file = (~file, string) => {
  let oc = open_out(file);
  output_string(oc, string);
  close_out(oc);
};

let read_validators = file => {
  let.await file_buffer = read_file(file);
  await(
    try({
      let json = Yojson.Safe.from_string(file_buffer);
      module T = {
        [@deriving of_yojson]
        type t = {
          address: Address.t,
          uri: Uri.t,
        };
      };
      let.ok validators = [%of_yojson: list(T.t)](json);
      Ok(List.map((T.{address, uri}) => (address, uri), validators));
    }) {
    | _ => Error("failed to parse json")
    },
  );
};
// Todo from Andre: we may have to do peer discovery?
// I don't know anything about that, except for having played with hyperswarm.
let validators = () =>
  [
    read_identity_file("0/identity.json"),
    read_identity_file("1/identity.json"),
    read_identity_file("2/identity.json"),
    read_identity_file("3/identity.json"),
  ]
  |> Lwt.all;

let validators_uris = () =>
  Lwt.map(List.map(validator => validator.uri), validators());

let make_filename_from_address = wallet_addr_str => {
  Printf.sprintf("%s.tzsidewallet", wallet_addr_str);
};

module Serializable = {
  [@deriving yojson]
  type wallet_file = {
    address: Wallet.t,
    priv_key: Address.key,
  };
};

let exits =
  Term.default_exits
  @ Term.[exit_info(1, ~doc="expected failure (might not be a bug)")];

// Commands
// ========

// create-wallet

let info_create_wallet = {
  let doc = "Creates a wallet file. The wallet file's filename is its address. The wallet file contains the private key corresponding to that address.";
  let man = [
    `S(Manpage.s_bugs),
    `P("Email bug reports to <contact@marigold.dev>."),
  ];
  Term.info("create-wallet", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let create_wallet = () => {
  let (key, wallet) = Wallet.make_wallet();

  let wallet_json =
    Serializable.wallet_file_to_yojson({priv_key: key, address: wallet});

  let wallet_addr_str = Wallet.address_to_string(wallet);
  let filename = make_filename_from_address(wallet_addr_str);

  try(`Ok(Yojson.Safe.to_file(filename, wallet_json))) {
  | _ => `Error((false, "Error writing JSON to file."))
  };
};

let create_wallet = Term.(ret(const(create_wallet) $ const()));

// create-transaction

let address = {
  let parser = string =>
    BLAKE2B_20.of_string(string)
    |> Option.map(Wallet.address_of_blake)
    |> Option.to_result(~none=`Msg("Expected a wallet address."));
  let printer = (fmt, wallet) =>
    Format.fprintf(fmt, "%s", Wallet.address_to_string(wallet));
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

let load_wallet_file = file => {
  let.ok wallet_yojson =
    try(Ok(file |> Yojson.Safe.from_file)) {
    | _ => Error(Printf.sprintf("failed to read JSON from file %s", file))
    };
  Serializable.wallet_file_of_yojson(wallet_yojson);
};
let info_create_transaction = {
  let doc =
    Printf.sprintf(
      "Submits a transaction to the sidechain. The transaction will be communicated to all known validators to be included in the next block. If the path to the wallet file corresponding to the sending address is not provided, a wallet file with the correct filename (%s) must be present in the current working directory",
      make_filename_from_address("address"),
    );
  let man = [
    `S(Manpage.s_bugs),
    `P("Email bug reports to <contact@marigold.dev>."),
  ];
  Term.info(
    "create-transaction",
    ~version="%‌%VERSION%%",
    ~doc,
    ~exits,
    ~man,
  );
};

let create_transaction = (sender_wallet_file, received_address, amount) => {
  let transaction = {
    let.ok wallet = load_wallet_file(sender_wallet_file);

    Ok(
      Operation.Side_chain.sign(
        ~secret=wallet.priv_key,
        ~nonce=0l,
        ~block_height=0L,
        ~source=wallet.address,
        ~amount,
        ~kind=Transaction({destination: received_address}),
      ),
    );
  };
  switch (transaction) {
  | Ok(transaction) =>
    // Broadcast transaction
    let.await validators_uris = validators_uris();
    let.await () =
      Networking.broadcast_operation_gossip_to_list(
        validators_uris,
        Networking.Operation_gossip.{operation: transaction},
      );

    Lwt.return(`Ok());
  | Error(err) => Lwt.return(`Error((false, err)))
  };
};

let lwt_ret = p => Term.(ret(const(Lwt_main.run) $ p));

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

  Term.(
    lwt_ret(const(create_transaction) $ address_from $ address_to $ amount)
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
  let man = [
    `S(Manpage.s_bugs),
    `P("Email bug reports to <contact@marigold.dev>."),
  ];
  Term.info("sign-block", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};
let sign_block = (key, block_hash) =>
  switch (load_wallet_file(key)) {
  | Ok(wallet) =>
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
  | Error(err) => Lwt.return(`Error((false, err)))
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
  let man = [
    `S(Manpage.s_bugs),
    `P("Email bug reports to <contact@marigold.dev>."),
  ];
  Term.info("produce-block", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let produce_block = (key, state_bin) =>
  switch (load_wallet_file(key)) {
  | Ok(wallet) =>
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
    Lwt.return(`Ok());
  | Error(err) => Lwt.return(`Error((false, err)))
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
  let man = [
    `S(Manpage.s_bugs),
    `P("Email bug reports to <contact@marigold.dev>."),
  ];
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
    Sys.mkdir(Printf.sprintf("./%d/", index), 0o700);
    let file = Printf.sprintf("./%d/%s", index, file);
    let uri = Printf.sprintf("http://localhost:%d", 4440 + index);

    let uri = Uri.of_string(uri);
    let (key, t) = Ed25519.generate();
    let identity = {key, t, uri};
    identity_to_yojson(identity)
    |> Yojson.Safe.pretty_to_string
    |> write_file(~file);
    (t, uri);
  };

  let make_validators_files = (file, validators, to_make) => {
    module T = {
      [@deriving to_yojson]
      type t = {
        address: Address.t,
        uri: Uri.t,
      };
    };

    let validator_mapping = List.map(((address, uri)) => T.{address, uri});

    List.iter(
      i => {
        validators
        |> validator_mapping
        |> [%to_yojson: list(T.t)]
        |> Yojson.Safe.pretty_to_string
        |> write_file(~file=Printf.sprintf("./%d/%s", i, file))
      },
      to_make,
    );
  };

  let to_make = [0, 1, 2, 3];

  Term.(
    const(() => {
      let validators =
        List.map(make_identity_file("identity.json"), to_make);
      make_validators_files("validators.json", validators, to_make);
    })
    $ const()
  );
};

// inject genesis block
let info_inject_genesis = {
  let doc = "Injects the genesis block";
  let man = [
    `S(Manpage.s_bugs),
    `P("Email bug reports to <contact@marigold.dev>."),
  ];
  Term.info("inject-genesis", ~version="%‌%VERSION%%", ~doc, ~exits, ~man);
};

let inject_genesis = {
  let make_new_block = validators => {
    let first = List.hd(validators);
    let state = Protocol.make(~initial_block=Block.genesis);
    let.await state = {
      let.await validators = read_validators("0/validators.json");
      let validators = Result.get_ok(validators);
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

// Term that just shows the help command, to use when no arguments are passed

let show_help = {
  let doc = "a tool for interacting with the WIP Tezos Sidechain";
  let sdocs = Manpage.s_common_options;
  let exits = Term.default_exits;
  let man = [
    `S(Manpage.s_bugs),
    `P("Email bug reports to <contact@marigold.dev>."),
  ];
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
    ],
  );
};
