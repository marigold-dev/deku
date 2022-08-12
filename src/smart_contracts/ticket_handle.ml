include Int32

module For_yojson = struct
  type t = Int32.t [@@deriving yojson]
end

include For_yojson

let to_bytes t =
  let buf = Bytes.create 4 in
  Bytes.set_int32_le buf 0 t;
  buf

let of_bytes t = Bytes.get_int32_le t 0
let to_string t = to_bytes t |> Bytes.unsafe_to_string
let of_string t = String.to_bytes t |> of_bytes
let size = 4
