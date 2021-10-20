open Tezos;
[@deriving yojson]
type t =
  Ticket.t = {
    ticketer: Address.t,
    data: bytes,
  };

// TODO: should we expose to_yojson here?
let compare = (a, b) => {
  module T = {
    [@deriving ord]
    type t = {
      ticketer: string,
      data: bytes,
    };
    let of_t = (Ticket.{ticketer, data}) => {
      ticketer: Address.to_string(ticketer),
      data,
    };
  };
  T.(compare(of_t(a), of_t(b)));
};
