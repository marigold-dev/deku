#import "errors.mligo" "Error"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"

type root_hash_action = Parameter.Types.root_hash_action
type vault_deposit = Parameter.Types.vault_deposit
type vault_withdraw = Parameter.Types.vault_withdraw
type root_hash = Parameter.Types.root_hash_storage
type vault = Parameter.Types.vault_storage

type action = Parameter.Types.t
type storage = Storage.Types.t

(* Update the corresponding root hash of Deku in this smart contract *)
let update_root_hash (root_hash_update : root_hash_action) (root_hash: root_hash) (vault : vault) =
  let root_hash = Storage.Utils.root_hash_main root_hash_update root_hash in
  let vault =
    Storage.Utils.vault_add_handles_hash
      root_hash.current_handles_hash
      vault in
  (root_hash, vault)

(* Deposit an amount of tickets to a Deku address *)
let deposit_tickets (deposit : vault_deposit) (vault : vault) = 
  Storage.Utils.vault_deposit deposit vault

(* Withdraw an amount of tickets from a Deku address *)
let withdraw_tickets (withdraw : vault_withdraw) (vault : vault) = 
  Storage.Utils.vault_withdraw withdraw vault

let main (action, storage : action * storage) =
  let { root_hash; vault; metadata } = storage in
  match action with
  | Update_root_hash root_hash_update -> 
    let (root_hash, vault) = update_root_hash root_hash_update root_hash vault in
    (([] : operation list), { root_hash = root_hash; vault = vault; metadata = metadata })
  | Deposit deposit -> 
    let vault = deposit_tickets deposit vault in
    (([] : operation list), { root_hash = root_hash; vault = vault; metadata = metadata })
  | Withdraw withdraw -> 
    let (operations, vault) = withdraw_tickets withdraw vault in
    (operations, { root_hash = root_hash; vault = vault; metadata = metadata })
    

