module type STRINGABLE = sig
  type t

  val name : string

  val of_string : string -> t option

  val to_string : t -> string
end

(** Implementation of bin_prot interfaces through string conversion *)
module String_like (P : STRINGABLE) = struct
  let bin_read_t buf ~pos_ref =
    match P.of_string @@ Bin_prot.Read.bin_read_string buf ~pos_ref with
    | None -> failwith "Invalid hex"
    | Some t -> t

  let bin_size_t t = Bin_prot.Std.bin_size_string @@ P.to_string t

  let bin_shape_t = Bin_prot.Shape.(basetype (Uuid.of_string P.name) [])

  let bin_reader_t =
    let variant_wrong_type name _buf ~pos_ref _x =
      Bin_prot.Common.raise_variant_wrong_type name !pos_ref in
    Bin_prot.Type_class.
      { read = bin_read_t; vtag_read = variant_wrong_type P.name }

  let __bin_read_t__ _buf ~pos_ref _vint =
    Bin_prot.Common.raise_variant_wrong_type P.name !pos_ref

  let bin_write_t buf ~pos t =
    Bin_prot.Write.bin_write_string buf ~pos (P.to_string t)

  let bin_writer_t =
    Bin_prot.Type_class.{ size = bin_size_t; write = bin_write_t }

  let bin_t =
    Bin_prot.Type_class.
      { shape = bin_shape_t; writer = bin_writer_t; reader = bin_reader_t }
end
