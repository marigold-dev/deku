module type S = sig
  include Conversions.S

  type t = Int64.t [@@deriving ord, eq]

  val to_string : t -> string

  val of_string : string -> t

  val size : int

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t
end

module Make (CC : Conversions.S) = struct
  include CC
  include Int64

  let to_bytes t =
    let buf = Bytes.create 8 in
    Bytes.set_int64_le buf 0 t;
    buf

  let of_bytes t = Bytes.get_int64_le t 0

  let size = 8
end
