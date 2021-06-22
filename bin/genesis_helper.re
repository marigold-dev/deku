open Helpers;
open Node;
open State;
open Protocol;

let read_file = file => {
  let.await lines =
    Lwt_io.with_file(~mode=Input, file, ic =>
      Lwt_io.read_lines(ic) |> Lwt_stream.to_list
    );
  await(lines |> String.concat("\n"));
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

let gen_credentials = () => {
  let write_file = (~file, string) => {
    let oc = open_out(file);
    output_string(oc, string);
    close_out(oc);
  };

  let make_identity_file = (~file, ~uri) => {
    open Mirage_crypto_ec;
    let uri = Uri.of_string(uri);
    let (key, t) = Ed25519.generate();
    let identity = {key, t, uri};
    identity_to_yojson(identity)
    |> Yojson.Safe.pretty_to_string
    |> write_file(~file);
    (t, uri);
  };

  let make_validators_file = (~file, ~validators) => {
    module T = {
      [@deriving to_yojson]
      type t = {
        address: Address.t,
        uri: Uri.t,
      };
    };
    validators
    |> List.map(((address, uri)) => T.{address, uri})
    |> [%to_yojson: list(T.t)]
    |> Yojson.Safe.pretty_to_string
    |> write_file(~file);
  };

  let validators = [
    make_identity_file(~file="identity_0.json", ~uri="http://localhost:4440"),
    make_identity_file(~file="identity_1.json", ~uri="http://localhost:4441"),
    make_identity_file(~file="identity_2.json", ~uri="http://localhost:4442"),
    make_identity_file(~file="identity_3.json", ~uri="http://localhost:4443"),
  ];
  make_validators_file(~file="validators.json", ~validators);
};

let inject_genesis = () => {
  let read_file = file => {
    let.await lines =
      Lwt_io.with_file(~mode=Input, file, ic =>
        Lwt_io.read_lines(ic) |> Lwt_stream.to_list
      );
    await(lines |> String.concat("\n"));
  };
  let read_file = file => read_file(file) |> Lwt_main.run;
  let read_identity_file = file => {
    let file_buffer = read_file(file);
    let json = Yojson.Safe.from_string(file_buffer);
    identity_of_yojson(json) |> Result.get_ok;
  };
  // let read_validators_file = file => {
  //   let file_buffer = read_file(file);
  //   let json = Yojson.Safe.from_string(file_buffer);
  //   [%of_yojson: list(Validators.validator)](json) |> Result.get_ok;
  // };
  let make_new_block = validators => {
    let first = List.nth(validators, 0);
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
    let signatures =
      validators
      |> List.map(validator => Block.sign(~key=validator.key, block));

    let.await () =
      validators
      |> Lwt_list.iter_p(validator =>
           Networking.post(
             (module Networking.Block_and_signature_spec),
             {block, signature: List.nth(signatures, 0)},
             validator.uri,
           )
         );
    validators
    |> Lwt_list.iter_p(validator =>
         signatures
         |> Lwt_list.iter_p(signature => {
              Lwt.catch(
                () =>
                  Networking.post(
                    (module Networking.Signature_spec),
                    Networking.Signature_spec.{
                      hash: block.Block.hash,
                      signature,
                    },
                    validator.uri,
                  ),
                _exn => Lwt.return_unit,
              )
            })
       );
  };

  let validators = [
    read_identity_file("0/identity.json"),
    read_identity_file("1/identity.json"),
    read_identity_file("2/identity.json"),
    // read_identity_file("identity_3.json"),
  ];
  make_new_block(validators);
};

Mirage_crypto_rng_unix.initialize();
if (Sys.argv[1] == "make-credentials") {
  gen_credentials();
} else if (Sys.argv[1] == "inject-genesis") {
  inject_genesis() |> Lwt_main.run;
};
