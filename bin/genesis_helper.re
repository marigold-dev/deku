open Helpers;
open Node;
open State;
open Protocol;

let gen_credentials = () => {
  let write_file = (~file, string) => {
    let oc = open_out(file);
    output_string(oc, string);
    close_out(oc);
  };

  let make_identity_file = (~file, ~uri) => {
    open Mirage_crypto_pk;
    let uri = Uri.of_string(uri);
    let key = Rsa.generate(~bits=2048, ());
    let t = Rsa.pub_of_priv(key);
    let identity = {key, t, uri};
    identity_to_yojson(identity)
    |> Yojson.Safe.pretty_to_string
    |> write_file(~file);
    Validators.{address: t, uri};
  };

  let make_validators_file = (~file, ~validators) =>
    [%to_yojson: list(Validators.validator)](validators)
    |> Yojson.Safe.pretty_to_string
    |> write_file(~file);

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
  // let read_validators_file = file => {
  //   let file_buffer = read_file(file);
  //   let json = Yojson.Safe.from_string(file_buffer);
  //   [%of_yojson: list(Validators.validator)](json) |> Result.get_ok;
  // };
  let make_genesis = validators => {
    let first = List.nth(validators, 0);
    let block =
      Block.make(
        ~author=first.t,
        ~block_height=1L,
        ~main_chain_ops=[],
        ~side_chain_ops=[],
      );
    let signatures =
      validators
      |> List.map(validator => {
           let multisig =
             Multisig.signatures(
               Signed.sign(~key=validator.key, block.hash)
               |> Multisig.of_signed,
             )
             |> List.nth(_, 0);
           Networking.{key: multisig.key, signature: multisig.signature};
         });

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
    read_identity_file("identity_0.json"),
    read_identity_file("identity_1.json"),
    read_identity_file("identity_2.json"),
    // read_identity_file("identity_3.json"),
  ];
  make_genesis(validators);
};

Mirage_crypto_rng_unix.initialize();
if (Sys.argv[1] == "make-credentials") {
  gen_credentials();
} else if (Sys.argv[1] == "inject-genesis") {
  inject_genesis() |> Lwt_main.run;
};
