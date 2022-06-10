type t [@@deriving yojson, ord, eq]

val to_string : t -> string

val of_string : string -> t option

val make : Address.t -> Tezos.Ticket_id.t -> Amount.t -> t

val to_bytes : t -> bytes

val of_bytes : bytes -> t option

module Table : sig
  include Hashtbl.S with type key := t
end
