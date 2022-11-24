module type OrderedType = sig
  include Map.OrderedType

  val encoding : t Data_encoding.t
end

module type S = sig
  include Set.S

  val encoding : t Data_encoding.t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
