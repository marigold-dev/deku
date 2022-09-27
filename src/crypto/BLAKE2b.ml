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
  let digest_size = P.digest_size

  module With_alg (Alg : Hash_intf.Alg) = struct
    let sign secret hash = Alg.sign secret (to_raw_string hash)

    let verify key signature hash =
      Alg.verify key signature (to_raw_string hash)
  end

  module With_b58 (P : sig
    val prefix : string
  end) =
  With_b58 (struct
    open P

    type t = hash

    let prefix = prefix
    let to_raw = to_raw
    let of_raw = of_raw
  end)

  (* TODO: expose this exception *)
  exception Not_a_hash

  let hash_of_yojson json =
    let string = [%of_yojson: string] json in
    match of_hex string with Some hash -> hash | None -> raise Not_a_hash

  let yojson_of_hash hash = [%yojson_of: string] (to_hex hash)

  module Set = Set.Make (struct
    type t = hash [@@deriving ord, yojson]
  end)

  module Map = Map.Make (struct
    type t = hash [@@deriving ord, yojson]
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

let yojson_of_t t = `String (to_hex t)

let t_of_yojson json =
  let json_str = Yojson.Safe.Util.to_string json in
  match json_str |> of_hex with
  | Some t -> t
  | None ->
      failwith (Format.sprintf "cannot deserialize %s to blake2b" json_str)
