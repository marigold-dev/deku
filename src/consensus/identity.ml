open Deku_crypto
open Deku_concepts

type identity = Identity of { secret : Secret.t; key : Key.t }
and t = identity

let key identity =
  let (Identity { secret = _; key }) = identity in
  key

let sign ~hash identity =
  let (Identity { secret; key = _ }) = identity in
  Verified_signature.sign hash secret
