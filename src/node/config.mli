type t = private {
  identity : Consensus.identity;
  minimum_block_delay : float;
}
[@@deriving yojson]

val make : identity:Consensus.identity -> minimum_block_delay:float -> t
