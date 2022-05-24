module PseudoEffect = struct
  type 'a return = { return : 'b. 'a -> 'b }

  let returner (type r) f =
    let exception Return of r in
    let p = { return = (fun x -> raise (Return x)) } in
    try f p with
    | Return r -> r
end

module Height = Int64 [@@deriving yojson]

module HeightMap = Map.Make (struct
  type t = Height.t
  let compare = compare
end)
