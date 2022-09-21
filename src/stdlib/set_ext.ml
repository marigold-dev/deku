module type OrderedType = sig
  include Map.OrderedType

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type S = sig
  include Set.S

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Make (Ord : OrderedType) = struct
  include Set.Make (Ord)

  let t_of_yojson json =
    let list = [%of_yojson: Ord.t list] json in
    List.fold_left (fun t elt -> add elt t) empty list

  let yojson_of_t t =
    let list = elements t in
    [%yojson_of: Ord.t list] list
end
