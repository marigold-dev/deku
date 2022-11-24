module type OrderedType = sig
  include Map.OrderedType

  val encoding : t Data_encoding.t
end

module type S = sig
  include Set.S

  val encoding : t Data_encoding.t
end

module Make (Ord : OrderedType) = struct
  include Set.Make (Ord)

  let of_list l = List.fold_left (fun t elt -> add elt t) empty l

  let encoding =
    let open Data_encoding in
    conv elements of_list (list Ord.encoding)
end
