open Tezos_interop;

[@deriving yojson]
type t =
  Ticket.t = {
    ticketer: Address.t,
    data: bytes,
  };

// TODO: should we expose to_yojson here?
let compare = (a, b) => {
  let to_string = a =>
    Bytes.to_string(a.data) ++ Tezos_interop.Address.to_string(b.ticketer);
  String.compare(to_string(a), to_string(b));
};
