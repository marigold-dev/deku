include Tezos.Ticket_id;
type t =
  Tezos.Ticket_id.t = {
    ticketer: Tezos.Address.t,
    data: bytes,
  };
