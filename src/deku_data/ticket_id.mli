type ticketer =
  | Deku  of Address.t
  | Tezos of Tezos.Address.t
[@@deriving ord, eq, yojson]

type t = {
  ticketer : ticketer;
  data : bytes;
}
[@@deriving eq, ord, yojson]

val to_tezos : t -> (Tezos.Ticket_id.t, [> `Not_supported]) result

val of_tezos : Tezos.Ticket_id.t -> (t, [> `Invalid_ticket]) result

val is_tezos : t -> bool

val mint_ticket : contract_address:Address.t -> data:bytes -> t

val to_string : t -> string

val of_string : string -> t option
