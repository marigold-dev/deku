module type OrderedType = sig
  include Map.OrderedType

  val encoding : t Data_encoding.t
end

module type S = sig
  include Map.S

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

module Make (Ord : OrderedType) : S with type key = Ord.t
