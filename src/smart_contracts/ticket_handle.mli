module type S = sig
  include Conversions.S

  type t [@@deriving yojson, ord, eq]

  val to_string : t -> string

  val of_string : string -> t option

  val make : Address.t -> Ticket_id.t -> Amount.t -> t

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t option

  module Table : sig
    include Hashtbl.S with type key := t
  end
end

module Make (CC : Conversions.S) :
  S with module Address = CC.Address and module Amount = CC.Amount
