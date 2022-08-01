type timestamp = float
type t = timestamp

let of_float float = float

let timeouts_since ~current ~since =
  let diff = current -. since in
  let skips = diff /. Deku_constants.block_timeout in
  Float.(to_int (floor skips))
