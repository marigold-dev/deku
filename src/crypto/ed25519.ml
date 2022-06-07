open Mirage_crypto_ec
open Ed25519
open Helpers

module Secret = struct
  type t = priv

  let equal a b =
    let a, b = (priv_to_cstruct a, priv_to_cstruct b) in
    Cstruct.equal a b

  let compare a b = Cstruct.compare (priv_to_cstruct a) (priv_to_cstruct b)

  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "Ed25519.Secret_key"

    let title = "An Ed25519 secret key"

    let size = 32

    let prefix = Base58.Prefix.ed25519_seed

    let to_raw t = Cstruct.to_string (Ed25519.priv_to_cstruct t)

    let of_raw string =
      Ed25519.priv_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)
end

module Key = struct
  type t = pub

  let bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t = failwith "Not used by Pollinate"
  let bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t= failwith "Not used by Pollinate"
  let __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t = failwith "Not used by Pollinate"
  
  let bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let bin_size_t : t -> int = failwith "Not used by Pollinate"
  let bin_shape_t : Bin_shape_lib.Bin_shape.t = failwith "Not used by Pollinate" 
  let bin_t : t -> int = failwith "Not used by Pollinate"

  let of_secret = Ed25519.pub_of_priv

  let equal a b =
    let a, b = (pub_to_cstruct a, pub_to_cstruct b) in
    Cstruct.equal a b

  let compare a b = Cstruct.compare (pub_to_cstruct a) (pub_to_cstruct b)

  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "Ed25519.Public_key"

    let title = "Ed25519 public key"

    let size = 32

    let prefix = Base58.Prefix.ed25519_public_key

    let to_raw t = Cstruct.to_string (Ed25519.pub_to_cstruct t)

    let of_raw string =
      Ed25519.pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)
end

module Key_hash = struct
  type t = BLAKE2B_20.t [@@deriving ord, eq, bin_io]

  let bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t = failwith "Not used by Pollinate"
  let bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t= failwith "Not used by Pollinate"
  let __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t = failwith "Not used by Pollinate"
  
  let bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let bin_size_t : t -> int = failwith "Not used by Pollinate"
  let bin_shape_t : Bin_shape_lib.Bin_shape.t = failwith "Not used by Pollinate" 
  let bin_t : t -> int = failwith "Not used by Pollinate"

  let of_key t = BLAKE2B_20.hash (Ed25519.pub_to_cstruct t |> Cstruct.to_string)

  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "Ed25519.Public_key_hash"

    let title = "An Ed25519 public key hash"

    let size = BLAKE2B_20.size

    let prefix = Base58.Prefix.ed25519_public_key_hash

    let to_raw = BLAKE2B_20.to_raw_string

    let of_raw = BLAKE2B_20.of_raw_string
  end)
end

module Signature = struct
  type t = string [@@deriving ord, eq]

  let bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t = failwith "Not used by Pollinate"
  let bin_writer_t : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let bin_reader_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t= failwith "Not used by Pollinate"
  let __bin_read_t__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t = failwith "Not used by Pollinate"
  
  let bin_write_t : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let __bin_write_t__ : Bin_prot.Common.buf -> pos:int -> t -> int = failwith "Not used by Pollinate"

  let bin_size_t : t -> int = failwith "Not used by Pollinate"
  let bin_shape_t : Bin_shape_lib.Bin_shape.t = failwith "Not used by Pollinate" 
  let bin_t : t -> int = failwith "Not used by Pollinate"

  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "Ed25519"

    let title = "An Ed25519 signature"

    let size = 64

    let prefix = Base58.Prefix.ed25519_signature

    let to_raw = Fun.id

    let of_raw string =
      match String.length string = size with
      | true -> Some string
      | false -> None
  end)

  let zero = String.make size '\x00'
end

let sign secret hash =
  Cstruct.of_string (BLAKE2B.to_raw_string hash)
  |> Ed25519.sign ~key:secret
  |> Cstruct.to_string

let verify public signature hash =
  verify ~key:public
    ~msg:(Cstruct.of_string (BLAKE2B.to_raw_string hash))
    (Cstruct.of_string signature)

let generate () = Ed25519.generate ()
