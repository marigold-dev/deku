type t = { identity : Consensus.identity } [@@deriving yojson]

let make ~identity = { identity }
