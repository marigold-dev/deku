open Helpers

module M = Map.Make_with_yojson(Address.Originated)
type contract_state = {
  entrypoint: string option;
  originator: Address.Implicit.t;
  storage: Interpreter.Types.Stack_item.t;
  code: Interpreter.Types.Program.t
} [@@deriving yojson]

type t = contract_state M.t [@@deriving to_yojson]
let empty : t =  M.empty
let add t address new_state = M.add address new_state t
let get t address = M.find_opt address t 
let update_entry t address f = M.update_entry address f t
let exists t address = M.exists address t
let make_state ~entrypoint ~originator ~storage ~code () = {
  entrypoint; originator; storage ; code
}
