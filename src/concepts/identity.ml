open Deku_crypto

type identity =
  | Identity of { secret : Secret.t; key : Key.t; key_hash : Key_hash.t }

and t = identity

let make secret =
  let key = Key.of_secret secret in
  let key_hash = Key_hash.of_key key in
  Identity { secret; key; key_hash }

let key identity =
  let (Identity { key; _ }) = identity in
  key

let key_hash identity =
  let (Identity { key_hash; _ }) = identity in
  key_hash

let sign ~hash identity =
  let (Identity { secret; key = _; _ }) = identity in
  Signature.sign secret hash

let t_of_yojson json =
  let secret = Secret.t_of_yojson json in
  make secret

let yojson_of_t identity =
  let (Identity { secret; _ }) = identity in
  Secret.yojson_of_t secret
