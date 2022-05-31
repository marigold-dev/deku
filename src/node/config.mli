type t = private { identity : Consensus.identity } [@@deriving yojson]

val make : identity:Consensus.identity -> t
