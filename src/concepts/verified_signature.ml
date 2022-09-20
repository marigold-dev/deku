open Deku_crypto

exception Invalid_hash
exception Invalid_signature

type verified_signature =
  | Verified_signature of {
      key : Key.t;
      (* denomarlization *)
      key_hash : Key_hash.t;
      signature : Signature.t;
      signed_hash : BLAKE2b.t;
    }

and t = verified_signature [@@deriving eq, ord]

let sign hash identity =
  let key = Identity.key identity in
  let key_hash = Identity.key_hash identity in
  let signature = Identity.sign ~hash identity in
  Verified_signature { signed_hash = hash; key; key_hash; signature }

let verify signed_hash key signature =
  let key_hash = Key_hash.of_key key in
  if Signature.verify key signature signed_hash then
    Some (Verified_signature { signed_hash; key; key_hash; signature })
  else None

let key signature =
  let (Verified_signature { key; _ }) = signature in
  key

let key_hash signature =
  let (Verified_signature { key_hash; _ }) = signature in
  key_hash

let signature signature =
  let (Verified_signature { signature; _ }) = signature in
  signature

let signed_hash signature =
  let (Verified_signature { signed_hash; _ }) = signature in
  signed_hash

module Repr = struct
  (* TODO: this encoding is not ideal *)
  type verified_signature = {
    key : Key.t;
    signature : Signature.t;
    signed_hash : string;
  }
  [@@deriving yojson]
end

let t_of_yojson json =
  let Repr.{ key; signature; signed_hash } =
    Repr.verified_signature_of_yojson json
  in
  match BLAKE2b.of_hex signed_hash with
  | Some hash -> (
      match verify hash key signature with
      | Some signature -> signature
      | None -> raise Invalid_signature)
  | None -> raise Invalid_hash

let yojson_of_t signature =
  let (Verified_signature { key; key_hash = _; signature; signed_hash }) =
    signature
  in
  let signed_hash = BLAKE2b.to_hex signed_hash in
  Repr.yojson_of_verified_signature { key; signature; signed_hash }

module Set = Set.Make (struct
  type t = verified_signature

  let compare = compare
end)

let pp fmt _t = Format.pp_print_string fmt "<opaque>"
