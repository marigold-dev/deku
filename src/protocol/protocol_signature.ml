open Helpers
open Crypto
type t = {
  signature : Signature.t;
  public_key : Wallet.t;
  address : Key_hash.t;
}
[@@deriving ord]
let public_key t = t.public_key
let address t = t.address
let signature t = t.signature
let to_yojson, of_yojson =
  let module Serialized_data = struct
    type t = {
      signature : Signature.t;
      public_key : Wallet.t;
    }
    [@@deriving yojson]
  end in
  let to_yojson t =
    Serialized_data.to_yojson
      { signature = t.signature; public_key = t.public_key } in
  let of_yojson json =
    let%ok { signature; public_key } = Serialized_data.of_yojson json in
    let address = Key_hash.of_key public_key in
    Ok { signature; public_key; address } in
  (to_yojson, of_yojson)
let sign ~key:secret hash =
  let signature = Signature.sign secret hash in
  let public_key = Key.of_secret secret in
  let address = Key_hash.of_key public_key in
  { signature; public_key; address }
let verify ~signature hash =
  Signature.verify signature.public_key signature.signature hash
module type S = sig
  type value
  type signature = t
  type t = private {
    value : value;
    signature : signature;
  }
  val sign : key:Secret.t -> value -> t
  val verify : signature:signature -> value -> bool
end
module Make (P : sig
  type t
  val hash : t -> BLAKE2B.t
end) =
struct
  type value = P.t
  type signature = t
  type t = {
    value : value;
    signature : signature;
  }
  let sign ~key value =
    let signature = P.hash value |> sign ~key in
    { value; signature }
  let verify ~signature value = P.hash value |> verify ~signature
end
