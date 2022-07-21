open Helpers
open Crypto
open Protocol

type t = {
  producer : Validators.validator;
  signature : Signature.t;
}

let make ~producer ~signature =
  let hash = Key_hash.to_string producer.Validators.address |> BLAKE2B.hash in
  let%assert () =
    (`Invalid_bootstrapper_signature, Signature.verify ~signature hash) in
  Ok { producer; signature }
