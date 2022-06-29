module type S = sig
  module Address : sig
    type t [@@deriving yojson, eq]

    val size : int

    val is_implicit : t -> bool

    val is_originated : t -> bool

    val of_bytes : bytes -> t

    val to_bytes : t -> bytes

    val to_string : t -> string
  end

  module Ticket_id : sig
    type t [@@deriving yojson]

    val size : t -> int

    val to_bytes : t -> bytes

    val mint_ticket : contract_address:Address.t -> data:bytes -> t
  end

  module Amount : sig
    type t [@@deriving yojson]

    val zero : t

    val equal : t -> t -> bool

    val ( - ) : t -> t -> t

    val ( + ) : t -> t -> t

    val size : int

    val of_int : int -> t

    val to_int : t -> int
  end
end
