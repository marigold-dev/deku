type identity = private {
  secret : Crypto.Secret.t;
  key : Crypto.Key.t;
  t : Crypto.Key_hash.t;
  uri : Uri.t;
}
[@@deriving yojson]

val make_identity :
  secret:Crypto.Secret.t -> key:Crypto.Key.t -> uri:Uri.t -> identity

type t = private {
  identity : identity;
  minimum_block_delay : float;
}
[@@deriving yojson]

val make : identity:identity -> minimum_block_delay:float -> t
