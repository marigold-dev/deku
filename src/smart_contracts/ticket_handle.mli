module type S = sig
  include Conversions.S

  type t = Int64.t [@@deriving ord, eq]

  val to_string : t -> string

  val of_string : string -> t

  val size : int

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t
end

module Make (CC : Conversions.S) :
  S
    with module Address = CC.Address
     and module Amount = CC.Amount
     and module Ticket_id = CC.Ticket_id
