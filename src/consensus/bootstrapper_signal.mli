type t = private {
  producer : Protocol.Validators.validator;
  signature : Protocol.Signature.t;
}

val make :
  producer:Protocol.Validators.validator ->
  signature:Protocol.Signature.t ->
  (t, [> `Invalid_bootstrapper_signature]) Result.t
