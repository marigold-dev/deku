module Result : sig
  include module type of Result

  module Let_syntax : sig
    val ( let* ) :
      ('a, 'b) result -> ('a -> ('weak1, 'b) result) -> ('weak1, 'b) result

    val ( let+ ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  end

  module Infix : sig
    val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

    val ( >>| ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  end

  val wrap : 'a 'b 'c. ('a, 'b) result -> f:('b -> 'c) -> ('a, 'c) result
end

module Option : sig
  include module type of Option

  module Let_syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Z : sig
  include module type of struct
    include Z
  end

  val yojson_of_t : t -> Yojson.Safe.t

  val t_of_yojson : Yojson.Safe.t -> t
end

module Map : sig
  include module type of Map

  module type S_with_yojson = sig
    include Map.S

    val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

    val t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
  end

  module Make_with_yojson : functor
    (K : sig
       type t [@@deriving ord, yojson]
     end)
    -> sig
    include Map.S with type key = K.t

    val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

    val t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
  end
end

module Set : sig
  include module type of Set

  module type S_with_yojson = sig
    include Set.S

    val yojson_of_t : t -> Yojson.Safe.t

    val t_of_yojson : Yojson.Safe.t -> t
  end

  module Make_with_yojson (K : sig
    type t [@@deriving ord, yojson]
  end) : sig
    include Set.S with type elt = K.t

    val yojson_of_t : t -> Yojson.Safe.t

    val t_of_yojson : Yojson.Safe.t -> t
  end
end
