open Tezos_interop;

[@deriving (ord, yojson)]
type t =
  Ticket.t = {
    ticketer: Tezos.Address.t,
    data: bytes,
  };
