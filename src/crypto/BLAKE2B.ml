open Helpers
open Bin_prot.Std

module Make (P : sig
  val size : int
end) : sig
  type t [@@deriving yojson, bin_io]

  val to_string : t -> string

  val of_string : string -> t option

  val of_raw_string : string -> t option

  val to_raw_string : t -> string

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : string -> t

  val hash_v : string list -> t

  val verify : hash:t -> string -> bool

  val both : t -> t -> t

  val size : int

  val pp : Format.formatter -> t -> unit

  module Map : Map.S with type key = t

  val encoding : t Data_encoding.t
end = struct
  include P

  include Digestif.Make_BLAKE2B (struct
    let digest_size = P.size
  end)

  let of_raw_string = of_raw_string_opt

  let to_string = to_hex

  let of_string string =
    let%some () =
      match String.length string = size * 2 with
      | true -> Some ()
      | false -> None in
    of_hex_opt string

  let to_yojson str = `String (to_hex str)

  let of_yojson json =
    let%ok hex = [%of_yojson: string] json in
    of_string hex |> Option.to_result ~none:"Invalid hex"

  let equal = equal

  let compare = unsafe_compare

  let hash data = digest_string data

  let hash_v lst = digestv_string lst

  let verify ~hash:expected_hash data = expected_hash = hash data

  let both a b = hash (to_raw_string a ^ to_raw_string b)

  let pp fmt t = Format.fprintf fmt "%s" (to_string t)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let encoding =
    let open Data_encoding in
    conv
      (fun hash -> to_raw_string hash)
      (fun string -> string |> of_raw_string |> Option.get)
      (Fixed.string size)

 let bin_read_t buf ~pos_ref =
   match of_string @@ Bin_prot.Read.bin_read_string buf ~pos_ref with
     | None -> failwith "Invalid hex"
     | Some t -> t
  let bin_size_t t = bin_size_string @@ to_hex t

  let bin_shape_t = Bin_prot.Shape.(basetype (Uuid.of_string "BLAKE2B") [])
  let bin_reader_t =
    let variant_wrong_type name _buf ~pos_ref _x =
      Bin_prot.Common.raise_variant_wrong_type name !pos_ref
    in
    Bin_prot.Type_class.{ read = bin_read_t; vtag_read = variant_wrong_type
  "BLAKE2B" }

  let __bin_read_t__ _ = Log.debug "bin_read_t called"; failwith "Not used by Pollinate"

  let bin_write_t buf ~pos t = Bin_prot.Write.bin_write_string buf ~pos (to_hex t)

  let bin_writer_t = Bin_prot.Type_class.{ size = bin_size_t; write = bin_write_t }
  let __bin_write_t__ _ = failwith "Not used by Pollinate"

  let bin_t = Bin_prot.Type_class.{ shape = bin_shape_t; writer = bin_writer_t; reader = bin_reader_t }
end

module BLAKE2B_20 = Make (struct
  let size = 20
end)

include Make (struct
  let size = 32
end)
