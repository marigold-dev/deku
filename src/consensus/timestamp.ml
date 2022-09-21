type timestamp = float
and t = timestamp [@@deriving yojson]

let of_float float = float
let to_float timestamp = timestamp

let timeouts_since ~current ~since =
  let diff = current -. since in
  let skips = diff /. Deku_constants.block_timeout in
  Float.(to_int (floor skips))
