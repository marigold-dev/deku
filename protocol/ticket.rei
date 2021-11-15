// TODO: at least do a proper abstraction here
[@deriving (ord, yojson)]
type t =
  Tezos.Ticket_id.t = {
    ticketer: Tezos.Address.t,
    data: bytes,
  };
