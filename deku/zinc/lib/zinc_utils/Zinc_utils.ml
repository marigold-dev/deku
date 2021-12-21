module Nothing = struct
  include Base.Nothing

  let to_yojson = unreachable_code

  let of_yojson _ = failwith "tried to create a void out of yojson"
end

module Z = struct
  include Z

  let to_yojson x = `String (Z.to_string x)

  let of_yojson = function
    | `String s -> Result.Ok (Z.of_string s)
    | _ -> Result.Error "JSON string expected"

  let pp : Format.formatter -> t -> unit =
   fun fmt v -> Format.fprintf fmt "%s" (Z.to_string v)
end

type label = int [@@deriving show {with_path = false}, eq, yojson]

type variant_label = string [@@deriving show {with_path = false}, eq, yojson]

module LMap = struct
  type 'a t = 'a array [@@deriving yojson, ord, eq, show]

  let of_list (lst : 'a list) : 'a t = lst |> Array.of_list

  let find arr item =
    try Some (Array.get arr item) with Invalid_argument _ -> None

  let add _k v array = Array.concat [array; [|v|]]

  let empty = [||]
end

module Blake2B_20 = Digestif.Make_BLAKE2B (struct
  let digest_size = 20
end)
