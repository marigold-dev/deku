open Crypto

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

module Make (CC : Conversions.S) = struct
  include CC
  include BLAKE2B_20

  let make owner ticket amount =
    BLAKE2B_20.hash_v
      [
        Address.to_string owner;
        Tezos.Ticket_id.to_string ticket;
        Int.to_string (Amount.to_int amount);
      ]

  let to_bytes t = to_string t |> Bytes.of_string

  let of_bytes t = Bytes.to_string t |> of_string

  module Table = Hashtbl.Make (struct
    type nonrec t = t

    let hash a = Hashtbl.hash a

    let equal a b = equal a b
  end)
end
