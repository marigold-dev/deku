#import "errors.mligo" "Errors"
#import "parameter.mligo" "Parameter"

type vault = Parameter.Types.vault
type vault_deposit = Parameter.Types.vault_deposit
type vault_storage = Parameter.Types.vault_storage

let vault_deposit (deposit: vault_deposit) (storage: vault_storage) =
  let { known_handles_hash; used_handles; vault } = storage in
  let (content, ticket) =  Tezos.read_ticket deposit.ticket in
  let (ticketer, (data, _)) = content in
  let (ticket, vault) =
    match
      Big_map.get_and_update
        (ticketer, data)
        (None: bytes ticket option)
        vault
    with
    | (None, vault) -> (ticket, vault)
    | (Some old_ticket, vault) ->
      (match Tezos.join_tickets (old_ticket, ticket) with
      | Some ticket -> (ticket, vault)
      | None -> ((failwith Errors.unreachable): (bytes ticket * vault))) in
  let vault = Big_map.add (ticketer, data) ticket vault in
  {
    known_handles_hash = known_handles_hash;
    used_handles = used_handles;
    vault = vault;
  }