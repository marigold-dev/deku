module rec V : sig
  type union =
    | Left of t
    | Right of t

  and t =
    | Int of Z.t
    | String of string
    | Bytes of bytes
    | Bool of int
    | Pair of t * t
    | Union of union
    | List of t list
    | Option of t option
    | Unit
    | Map of t Map.t
    | Set of Set.t
  [@@deriving ord, eq, yojson]

  val pp : Format.formatter -> t -> unit
end

and Map : (Helpers.Map.S_with_yojson with type key = V.t)
and Set : (Helpers.Set.S_with_yojson with type elt = V.t)

include module type of struct
  include V
end
