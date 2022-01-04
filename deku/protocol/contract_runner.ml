open Helpers
open Crypto
open Tezos



let make ~state () = 
  (module struct
  let state = state 
  let key_hash = Key_hash.of_key
  let get_contract_opt = function 
   | Address.Originated({contract; entrypoint = _}) -> 
    let open Option.Syntax in 
    let* contract_state = Contract_storage.get state.State.contracts_storage contract in
    let entrypoint = contract_state.entrypoint in  
    Contract.make (contract, entrypoint)
    |> Option.some
   | Implicit _ -> None
  let chain_id = Block.genesis.hash
  end : Interpreter.Executor)
