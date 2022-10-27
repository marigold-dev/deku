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

module Make (Ord : OrderedType) = struct
  include Map.Make (Ord)

  let of_list l = List.fold_left (fun t (key, value) -> add key value t) empty l

  let encoding (type a) (a_encoding : a Data_encoding.t) =
    let open Data_encoding in
    conv bindings of_list (list (tup2 (dynamic_size Ord.encoding) a_encoding))

  let t_of_yojson (type a) (a_of_yojson : _ -> a) json =
    let list = [%of_yojson: (Ord.t * a) list] json in
    List.fold_left (fun t (key, value) -> add key value t) empty list

  let yojson_of_t (type a) (yojson_of_a : a -> _) t =
    let list = bindings t in
    [%yojson_of: (Ord.t * a) list] list
end
