// TODO: at least do a proper abstraction here
[@deriving (ord, yojson)]
type t =
  Tezos_interop.Ticket.t = {
    ticketer: Tezos.Address.t,
    data: bytes,
  };
