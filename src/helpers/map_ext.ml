include Map
module Make_with_yojson (K : sig
  include Map.OrderedType
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end) =
struct
  include Map.Make (K)
  let to_yojson f t = t |> bindings |> [%to_yojson: (K.t * 'a) list] f
  let of_yojson f json =
    json
    |> [%of_yojson: (K.t * 'a) list] f
    |> Result.map (fun l -> l |> List.to_seq |> of_seq)
end
