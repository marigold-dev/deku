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
    (([] : operation list), { root_hash = root_hash; vault = vault })

(* Deposit an amount of tickets to a Deku address *)
let deposit_tickets (deposit : vault_deposit) (root_hash: root_hash) (vault : vault) = 
  let vault = Storage.Utils.vault_deposit deposit vault in
  (([] : operation list), { root_hash = root_hash; vault = vault; })

(* Withdraw an amount of tickets from a Deku address *)
let withdraw_tickets (withdraw : vault_withdraw) (root_hash: root_hash) (vault : vault) = 
  let (operations, vault) = Storage.Utils.vault_withdraw withdraw vault in
  (operations, { root_hash = root_hash; vault = vault; })

let main (action, storage : action * storage) =
  let { root_hash; vault } = storage in
  match action with
  | Update_root_hash root_hash_update -> update_root_hash root_hash_update root_hash vault
  | Deposit deposit -> deposit_tickets deposit root_hash vault
  | Withdraw withdraw -> withdraw_tickets withdraw root_hash vault
    

