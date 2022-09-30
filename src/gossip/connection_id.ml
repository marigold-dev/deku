type connection_id = int
and t = connection_id [@@deriving show, eq, ord]

let initial = 0
let next id = id + 1

module Map = Map.Make (struct
  type t = connection_id [@@deriving ord]
end)
