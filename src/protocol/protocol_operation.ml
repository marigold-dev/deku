open Helpers
open Crypto
open Core
module Consensus = struct
  type t =
    | Add_validator    of Validators.validator
    | Remove_validator of Validators.validator
  [@@deriving eq, ord, yojson]
  let hash payload = to_yojson payload |> Yojson.Safe.to_string |> BLAKE2B.hash
  let sign secret t =
    let hash = hash t in
    Signature.sign secret hash
  let verify key signature t =
    let hash = hash t in
    Signature.verify key signature hash
end
module Core_tezos = struct
  type t = Tezos_operation.t [@@deriving eq, ord, yojson]
end
module Core_user = struct
  type t = {
    hash : BLAKE2B.t;
    key : Key.t;
    signature : Signature.t;
    nonce : int32;
    block_height : int64;
    data : User_operation.t;
  }
  [@@deriving eq, yojson]
  let compare a b = BLAKE2B.compare a.hash b.hash
  let hash, verify =
    let apply f ~nonce ~block_height ~data =
      let to_yojson = [%to_yojson: int32 * int64 * User_operation.t] in
      let json = to_yojson (nonce, block_height, data) in
      let payload = Yojson.Safe.to_string json in
      f payload in
    let hash = apply BLAKE2B.hash in
    let verify ~hash = apply (BLAKE2B.verify ~hash) in
    (hash, verify)
  let sign ~secret ~nonce ~block_height ~data =
    let open User_operation in
    let key = Key.of_secret secret in
    match Address.to_key_hash data.sender with
    | Some sender when Key_hash.matches_key key sender ->
      let hash = hash ~nonce ~block_height ~data in
      let signature = Signature.sign secret hash in
      { hash; key; signature; nonce; block_height; data }
    | _ ->
      failwith
        "Operations to be signed should come from a key_hash that matches the \
         secret"
  let verify ~hash ~key ~signature ~nonce ~block_height ~data =
    let%ok sender =
      match Address.to_key_hash data.User_operation.sender with
      | Some sender -> Ok sender
      | _ -> Error "Sender should be a key_hash" in
    let%assert () =
      ( "Invalid core_user operation hash",
        verify ~hash ~nonce ~block_height ~data ) in
    let%assert () = ("Invalid core_user key", Key_hash.matches_key key sender) in
    let%assert () =
      ("Invalid core_user signature", Signature.verify key signature hash) in
    Ok { hash; key; signature; nonce; block_height; data }
  let of_yojson json =
    let%ok { hash; key; signature; nonce; block_height; data } =
      of_yojson json in
    verify ~hash ~key ~signature ~nonce ~block_height ~data
  let unsafe_make ~hash ~key ~signature ~nonce ~block_height ~data =
    { hash; key; signature; nonce; block_height; data }
end
type t =
  | Core_tezos of Core.Tezos_operation.t
  | Core_user  of Core_user.t
  | Consensus  of Consensus.t
[@@deriving eq, ord, yojson]
