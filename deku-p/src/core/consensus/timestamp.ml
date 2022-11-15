open Float

type timestamp = float
and t = timestamp [@@deriving show, yojson]

let encoding = Data_encoding.float
let block_timeout = Deku_constants.block_timeout
let genesis = Deku_constants.genesis_time
let of_float float = float
let to_float timestamp = timestamp

let timeouts_since ~current ~since =
  (* FIXME: ensure this is a nat *)
  let diff = current -. since in
  let skips = diff /. block_timeout in
  to_int (floor skips)

let next_timeout ~current ~since =
  let diff = current -. since in
  let skips = floor (diff /. block_timeout) in
  let skips = floor (skips +. 1.0) in
  since +. (skips *. block_timeout)
