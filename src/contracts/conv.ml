module type S = sig
  module Address : sig
    type t

    val size : int

    val of_bytes : bytes -> t

    val to_bytes : t -> bytes
  end

  module Ticket_handle : sig
    type t

    val size : int

    val of_bytes : bytes -> t

    val to_bytes : t -> bytes
  end

  module Ticket_id : sig
    type t

    val size : t -> int

    val to_bytes : t -> bytes
  end

  module Amount : sig
    type t

    val size : int

    val of_int : int -> t

    val to_int : t -> int
  end

  module Table : sig
    type t
  end

  module Operation : sig
    type t
  end
end
