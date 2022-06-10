open Helpers

module Map_yojson = Map.Make_with_yojson (struct
  type t = int [@@deriving yojson]

  let compare = Int.compare
end)

module Make_binable (K : sig
  type t

  val to_string : t -> string

  val of_string : string -> t option

  val name : string
end) : Core.Binable.S with type t := K.t = struct
  open Core
  open K

  let bin_size_t t = to_string t |> String.bin_size_t

  let bin_write_t buf ~pos t = String.bin_write_t buf ~pos (to_string t)

  let bin_read_t buf ~pos_ref =
    String.bin_read_t buf ~pos_ref |> of_string |> Option.value_exn

  let bin_shape_t = String.bin_shape_t

  let bin_writer_t =
    Bin_prot.Type_class.{ write = bin_write_t; size = bin_size_t }

  let variant_wrong_type name _buf ~pos_ref _x =
    Bin_prot.Common.raise_variant_wrong_type name !pos_ref

  let bin_reader_t =
    Bin_prot.Type_class.
      { read = bin_read_t; vtag_read = variant_wrong_type name }

  let bin_t =
    Bin_prot.Type_class.
      {
        reader = bin_reader_t;
        writer = bin_writer_t;
        shape = Bin_prot.Shape.bin_shape_string;
      }

  let __bin_read_t__ = variant_wrong_type name
end

module BLAKE2B : sig
  include module type of Crypto.BLAKE2B

  include Core.Binable.S with type t := t
end = struct
  include Crypto.BLAKE2B

  include Make_binable (struct
    include Crypto.BLAKE2B

    let name = "blake2b"
  end)
end

(* Key: Int, Value: Blake2b hash*)
module Inner_map = struct
  open Core
  include Map.Make_binable_using_comparator (Int)

  type t = BLAKE2B.t Map.M(Int).t [@@deriving bin_io]
end

(* Key: Int, Value: Small map*)
module Outer_map = struct
  open Core
  include Map.Make_binable_using_comparator (Int)

  type t = Inner_map.t Map.M(Int).t [@@deriving bin_io]
end

module Encoding = struct
  let ( let* ) = Option.bind

  open Bin_prot.Utils
  module BCommon = Bin_prot.Common

  let size_header_length = 8

  let read_all reader buf = reader buf ~pos_ref:(ref 0)

  let read_size_header byte_buff =
    let buf = BCommon.create_buf size_header_length in
    BCommon.blit_bytes_buf byte_buff buf ~len:size_header_length;
    read_all bin_read_size_header buf

  let prepare_buff byte_buff =
    let len = Bytes.length byte_buff - size_header_length in
    let buf = BCommon.create_buf len in
    BCommon.blit_bytes_buf ~src_pos:size_header_length byte_buff buf ~len;
    buf

  let pack writer payload =
    let buf = bin_dump ~header:true writer payload in
    let len = BCommon.buf_len buf in
    let byte_buff = Bytes.create len in
    BCommon.blit_buf_bytes buf byte_buff ~len;
    byte_buff

  let unpack reader payload =
    let buf = prepare_buff payload in
    read_all reader buf
end
