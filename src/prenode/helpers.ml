module Height = Int64 [@@deriving yojson]

module HeightMap = Map.Make (struct
  type t = Height.t
  let compare = compare
end)
