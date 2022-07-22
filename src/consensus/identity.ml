open Deku_crypto
open Deku_concepts

type identity = Secret.t
and t = identity

let sign ~hash identity = Verified_signature.sign hash identity
