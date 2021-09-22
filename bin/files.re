open Helpers;
open Node;
open Node_state;
open Protocol;

exception Invalid_json(string);
let read_json = (of_yojson, ~file) => {
  let.await string = Lwt_io.with_file(~mode=Input, file, Lwt_io.read);
  let json = Yojson.Safe.from_string(string);
  switch (of_yojson(json)) {
  | Ok(data) => await(data)
  | Error(error) => raise(Invalid_json(error))
  };
};

let write_json = (to_yojson, data, ~file) =>
  Lwt_io.with_file(~mode=Output, file, oc =>
    Lwt_io.write(oc, Yojson.Safe.pretty_to_string(to_yojson(data)))
  );

module Identity = {
  let read = read_json(identity_of_yojson);
  let write = write_json(identity_to_yojson);
};

module Wallet = {
  [@deriving yojson]
  type t = {
    address: Wallet.t,
    priv_key: Address.key,
  };
  let read = read_json(of_yojson);
  let write = write_json(to_yojson);
};
module Validators = {
  [@deriving yojson]
  type t = {
    address: Address.t,
    uri: Uri.t,
  };
  let read =
    read_json(json => {
      let.ok validators = [%of_yojson: list(t)](json);
      Ok(List.map(({address, uri}) => (address, uri), validators));
    });
  let write =
    write_json(validators =>
      validators
      |> List.map(((address, uri)) => {address, uri})
      |> [%to_yojson: list(t)]
    );
};

module Interop_context = {
  module Secret = {
    include Tezos_interop.Secret;
    let of_yojson =
      fun
      | `String(string) =>
        of_string(string) |> Option.to_result(~none="invalid secret")
      | _ => Error("expected a string");
    let to_yojson = t => `String(to_string(t));
  };

  [@deriving yojson]
  type t =
    Tezos_interop.Context.t = {
      rpc_node: Uri.t,
      secret: Secret.t,
      consensus_contract: Tezos_interop.Address.t,
      required_confirmations: int,
    };

  let read = read_json(of_yojson);
  let write = write_json(to_yojson);
};

module State_bin = {
  let read = (~file) =>
    Lwt_io.with_file(~mode=Input, file, Lwt_io.read_value);
  let write = (protocol, ~file) =>
    Lwt_io.with_file(~mode=Output, file, Lwt_io.write_value(_, protocol));
};

module Trusted_validators_membership_change = {
  [@deriving yojson]
  type t = Node.Trusted_validators_membership_change.t;

  let read = read_json([%of_yojson: list(t)]);
  let write = write_json([%to_yojson: list(t)]);
};
