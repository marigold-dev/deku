open Crypto

type t = private {
  identity : Consensus.identity;
  minimum_block_delay : float;
  bootstrapper : Key.t;
}
[@@deriving yojson]

val make :
  identity:Consensus.identity ->
  minimum_block_delay:float ->
  bootstrapper:Key.t ->
  t
