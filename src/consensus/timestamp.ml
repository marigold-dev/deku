type timestamp = float
and t = timestamp [@@deriving show, yojson]

let genesis = Deku_constants.genesis_time
let of_float float = float
let to_float timestamp = timestamp

let timeouts_since ~current ~since =
  (* FIXME: ensure this is a nat *)
  let diff = current -. since in
  let skips = diff /. Deku_constants.block_timeout in
  Float.(to_int (floor skips))
