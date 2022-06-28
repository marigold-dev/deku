#import "errors.mligo" "Error"
#import "parameter.mligo" "Parameter"
#import "update_root_hash.mligo" "Update_root_hash"
#import "deposit.mligo" "Deposit"
#import "withdraw.mligo" "Withdraw"

type storage = {
    root_hash: Parameter.Types.root_hash_storage;
    vault: Parameter.Types.vault_storage;
  }

type action =
| Update_root_hash of Parameter.Types.root_hash_action
| Deposit of Parameter.Types.vault_deposit
| Withdraw of Parameter.Types.vault_withdraw

let main (action, storage : action * storage) =
  let { root_hash; vault } = storage in
  match action with
  | Update_root_hash root_hash_update ->
    let root_hash = Update_root_hash.root_hash_main root_hash_update root_hash in
    let vault =
      Update_root_hash.vault_add_handles_hash
        root_hash.current_handles_hash
        vault in
    (([] : operation list), { root_hash = root_hash; vault = vault })
  | Deposit deposit ->
      let vault = Deposit.vault_deposit deposit vault in
      (([] : operation list), { root_hash = root_hash; vault = vault; })
  | Withdraw withdraw ->
    let (operations, vault) = Withdraw.vault_withdraw withdraw vault in
    (operations, { root_hash = root_hash; vault = vault; })

