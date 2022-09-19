type request_id = int
and t = request_id [@@deriving eq, ord]

let initial = 0
let next id = id + 1

module Map = Map.Make (struct
  type t = request_id

  let compare = compare
end)
