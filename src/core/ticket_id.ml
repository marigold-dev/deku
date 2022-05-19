type t = Tezos.Ticket_id.t = {
  ticketer : Tezos.Address.t;
  data : bytes;
}
[@@deriving eq, ord, yojson]
