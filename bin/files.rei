open Node;
open Node_state;
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

module Interop_context: {
  let read: (~file: string) => Lwt.t(Tezos_interop.Context.t);
  let write: (Tezos_interop.Context.t, ~file: string) => Lwt.t(unit);
};

module State_bin: {
  let read: (~file: string) => Lwt.t(Protocol.t);
  let write: (Protocol.t, ~file: string) => Lwt.t(unit);
};

module Trusted_validators_membership_change: {
  [@deriving yojson]
  type t = Node.Trusted_validators_membership_change.t;

  let read: (~file: string) => Lwt.t(list(t));
  let write: (list(t), ~file: string) => Lwt.t(unit);
};
