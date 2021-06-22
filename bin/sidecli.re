open Helpers;
open Node;
open State;
open Protocol;
open Cmdliner;

Printexc.record_backtrace(true);

// Helpers

let read_file = file => {
  let.await ic = Lwt_io.open_file(~mode=Input, file);
  let.await lines = Lwt_io.read_lines(ic) |> Lwt_stream.to_list;
  let.await () = Lwt_io.close(ic);
  await(lines |> String.concat("\n"));
};
let read_file = file => read_file(file) |> Lwt_main.run;
let read_identity_file = file => {
  let file_buffer = read_file(file);
  let json = Yojson.Safe.from_string(file_buffer);
  identity_of_yojson(json) |> Result.get_ok;
};

// Todo from Andre: we may have to do peer discovery?
// I don't know anything about that, except for having played with hyperswarm.
let validators = () => [
  read_identity_file("0/identity.json"),
  // read_identity_file("1/identity.json"),
  read_identity_file("2/identity.json"),
  // read_identity_file("identity_3.json"),
];

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

// Create wallet

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

  let swallet: Serializable.wallet_file = {priv_key: key, address: wallet};
  let to_yojson = Serializable.wallet_file_to_yojson;
  let wallet_json = swallet |> to_yojson;

  let wallet_addr_str = Wallet.address_to_string(wallet);

  let filename = make_filename_from_address(wallet_addr_str);

  try(`Ok(Yojson.Safe.to_file(filename, wallet_json))) {
  | _ => `Error((false, "Error writing JSON to file."))
  };
};

let create_wallet_t = Term.(ret(const(create_wallet) $ const()));

// Transactions

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

let create_transaction = (address_sender_str, address_receiver_str, amount) =>
  switch (
    address_sender_str == "",
    address_receiver_str == "",
    amount <= 0,
    address_receiver_str
    |> BLAKE2B.of_string
    |> Option.map(Wallet.address_of_blake),
  ) {
  | (true, _, _, _) =>
    Lwt.return(
      `Error((
        false,
        "Must provide a sender wallet address, or a path to a wallet file.",
      )),
    )
  | (_, true, _, _) =>
    Lwt.return(`Error((false, "Must provide a destination wallet address.")))
  | (_, _, true, _) =>
    Lwt.return(
      `Error((false, "Must provide a transaction amount above zero.")),
    )
  | (_, _, _, None) =>
    Lwt.return(
      `Error((false, "Transaction destination must be a valid address.")),
    )
  | (_, _, _, Some(address_to)) =>
    let transaction = {
      let input_file =
        switch (
          address_sender_str
          |> BLAKE2B.of_string
          |> Option.map(Wallet.address_of_blake)
        ) {
        | Some(_) => address_sender_str |> make_filename_from_address
        | None => address_sender_str
        };

      let.ok wallet_yojson =
        try(Ok(input_file |> Yojson.Safe.from_file)) {
        | _ =>
          Error(
            Printf.sprintf("failed to read JSON from file %s", input_file),
          )
        };

      let.ok wallet = Serializable.wallet_file_of_yojson(wallet_yojson);

      Ok(
        Operation.self_sign_side(
          ~key=wallet.priv_key,
          Operation.Side_chain.make(
            ~nonce=0l,
            ~block_height=0L,
            ~source=wallet.address,
            ~amount=Amount.of_int(amount),
            ~kind=Transaction({destination: address_to}),
          ),
        ),
      );
    };
    switch (transaction) {
    | Ok(transaction) =>
      let validators_uris =
        List.map(validator => validator.uri, validators());

      // Broadcast transaction
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

let create_transaction_t = {
  let address_from = {
    let doc = "The sending address, or a path to a wallet. If a bare sending address is provided, the corresponding wallet is assumed to be in the working directory.";
    let env = Arg.env_var("SENDER", ~doc);
    let doc = "The message to print.";
    Arg.(value & pos(0, string, "") & info([], ~env, ~docv="MSG", ~doc));
  };

  let address_to = {
    let doc = "The receiving address.";
    let env = Arg.env_var("RECEIVER", ~doc);
    let doc = "The message to print.";
    Arg.(value & pos(1, string, "") & info([], ~env, ~docv="MSG", ~doc));
  };

  let amount = {
    let doc = "The amount to be transacted.";
    let env = Arg.env_var("TRANSFER_AMOUNT", ~doc);
    let doc = "The message to print.";
    Arg.(value & pos(2, int, 0) & info([], ~env, ~docv="MSG", ~doc));
  };

  Term.(
    lwt_ret(const(create_transaction) $ address_from $ address_to $ amount)
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
    Term.(ret(const(() => `Help((`Pager, None))) $ const())),
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
      (create_wallet_t, info_create_wallet),
      (create_transaction_t, info_create_transaction),
    ],
  );
};
