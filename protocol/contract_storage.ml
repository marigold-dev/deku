open Helpers

module M = Map.Make_with_yojson(Tezos.Contract_hash)
type contract_state = {
  entrypoint: string option;
  originator: Address.t;
  storage: unit; (* zinc.t *)
  code: unit (* zinc.t *)
} [@@deriving to_yojson]

type t = {
   storage: contract_state M.t
}[@@deriving to_yojson]

let empty : t = {storage = M.empty}
