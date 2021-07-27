open Helpers;
open Node;
open State;
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
