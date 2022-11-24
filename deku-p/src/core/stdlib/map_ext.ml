module type OrderedType = sig
  include Map.OrderedType

  val encoding : t Data_encoding.t
end

module type S = sig
  include Map.S

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

module Make (Ord : OrderedType) = struct
  include Map.Make (Ord)

  let of_list l = List.fold_left (fun t (key, value) -> add key value t) empty l

  let encoding (type a) (a_encoding : a Data_encoding.t) =
    let open Data_encoding in
    conv bindings of_list (list (tup2 (dynamic_size Ord.encoding) a_encoding))
end
