open Deku_stdlib
open Deku_repr

module Make (P : sig
  val digest_size : int
end) =
struct
  type hash = Digestif.Make_BLAKE2B(P).t

  include Digestif.Make_BLAKE2B (P)

  let pp = pp
  let show = to_hex

  (* TODO: this compare is unsafe*)
  let compare a b = unsafe_compare a b
  let compare_hash = compare
  let of_raw string = of_raw_string_opt string
  let to_raw hash = to_raw_string hash

  let of_hex string =
    let size = digest_size * 2 in
    match String.length string = size with
    | true -> of_hex_opt string
    | false -> None

  let to_hex hash = to_hex hash
  let hash data = digest_string data
  let both a b = hash (to_raw_string a ^ to_raw_string b)

  let all l =
    let l = List.map to_raw_string l in
    hash (String.concat "" l)

  let digest_size = P.digest_size

  module With_alg (Alg : Hash_intf.Alg) = struct
    let sign secret hash = Alg.sign secret (to_raw_string hash)

    let verify key signature hash =
      Alg.verify key signature (to_raw_string hash)
  end

  module With_b58_and_encoding_and_yojson (P : sig
    val name : string
    val prefix : Prefix.t
  end) =
  struct
    include With_b58_and_encoding_and_yojson (struct
      include P

      type t = hash

      let size = digest_size
      let to_raw = to_raw
      let of_raw = of_raw
    end)

    module Set = Set.Make (struct
      type t = hash [@@deriving ord]

      let encoding = encoding
    end)

    module Map = Map.Make (struct
      type t = hash [@@deriving ord]

      let encoding = encoding
    end)
  end
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

let encoding =
  let open Data_encoding in
  conv
    (fun hash -> to_hex hash)
    (fun hex ->
      match of_hex hex with
      | Some hash -> hash
      | None -> failwith "impossible to decode")
    string
