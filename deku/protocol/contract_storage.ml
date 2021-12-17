open Helpers

module M = Map.Make_with_yojson(Tezos.Contract_hash)
type contract_state = {
  entrypoint: string option;
  originator: Address.t;
  storage: Interpreter.Types.Program.t; (* zinc.t *)
  code: Interpreter.Types.Program.t (* zinc.t *)
} [@@deriving yojson]

type t = contract_state M.t [@@deriving to_yojson]
let empty : t =  M.empty
let add t address new_state = M.add address new_state t
let make_state ~entrypoint ~originator ~storage ~code () = {
  entrypoint; originator; storage ; code
}
