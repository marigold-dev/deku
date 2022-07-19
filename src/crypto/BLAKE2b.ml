module Make (P : sig
  val digest_size : int
end) =
struct
  type hash = Digestif.Make_BLAKE2B(P).t

  include Digestif.Make_BLAKE2B (P)

  let pp fmt hash = pp fmt hash
  let show hash = to_hex hash

  (* TODO: this compare is unsafe*)
  let compare a b = unsafe_compare a b
  let of_raw string = of_raw_string_opt string
  let to_raw hash = to_raw_string hash

  let of_hex string =
    let size = digest_size * 2 in
    if String.length string = size then of_hex_opt string else None

  let to_hex hash = to_hex hash
  let hash data = digest_string data
  let both a b = hash (to_raw_string a ^ to_raw_string b)

  module With_alg (Alg : Hash_intf.Alg) = struct
    let sign secret hash = Alg.sign secret (to_raw_string hash)

    let verify key signature hash =
      Alg.verify key signature (to_raw_string hash)
  end

  module With_b58 (P : sig
    val prefix : string
  end) =
  Base58.Make (struct
    open P

    type t = hash

    let prefix = prefix
    let to_raw = to_raw
    let of_raw = of_raw
  end)
end

module Size_160 = struct
  let digest_size = 160 / 8
end

module Size_256 = struct
  let digest_size = 256 / 8
end

module BLAKE2b_160 = Make (Size_160)
module BLAKE2b_256 = Make (Size_256)
include BLAKE2b_256
