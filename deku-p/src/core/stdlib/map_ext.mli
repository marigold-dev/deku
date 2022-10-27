module type OrderedType = sig
  include Map.OrderedType

  val encoding : t Data_encoding.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type S = sig
  include Map.S

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  val t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
  val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
end

module Make (Ord : OrderedType) : S with type key = Ord.t
