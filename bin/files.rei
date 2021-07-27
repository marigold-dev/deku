open Node;
open State;
open Protocol;

exception Invalid_json(string);

module Identity: {
  let read: (~file: string) => Lwt.t(identity);
  let write: (identity, ~file: string) => Lwt.t(unit);
};

module Wallet: {
  type t = {
    address: Wallet.t,
    priv_key: Address.key,
  };
  let read: (~file: string) => Lwt.t(t);
  let write: (t, ~file: string) => Lwt.t(unit);
};

module Validators: {
  let read: (~file: string) => Lwt.t(list((Address.t, Uri.t)));
  let write: (list((Address.t, Uri.t)), ~file: string) => Lwt.t(unit);
};
