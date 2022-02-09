include Set
module Make_with_yojson (V : sig
  include Set.OrderedType
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end) =
struct
  include Set.Make (V)
  let to_yojson t = t |> to_seq |> List.of_seq |> [%to_yojson: V.t list]
  let of_yojson json = json |> [%of_yojson: V.t list] |> Result.map of_list
end
