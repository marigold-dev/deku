(** This contract is designed as a helper
    It provides two functions
    - Mint dummy tickets and deposit on Deku
    - Burn tickets received from Deku
    - Execute withdraws on Deku *)

type blake2b = bytes
type vault_ticket = bytes ticket
type vault_deposit = { ticket : vault_ticket; address : key_hash }

type vault_handle_id = nat
type vault_handle_structure = {
  id : vault_handle_id;
  owner : address;
  amount : nat;
  ticketer : address;
  (* TODO: probably data_hash *)
  data : bytes;
}
type vault_handle_proof = (blake2b * blake2b) list
type vault_withdraw = {
  handles_hash : blake2b;
  handle : vault_handle_structure;
  proof : vault_handle_proof;
  callback : vault_ticket contract;
}

type parameter =
  | Mint_to_deku of {
      deku_consensus : address;
      deku_recipient : key_hash;
      ticket_data : bytes;
      ticket_amount : nat;
    }
  | Withdraw_from_deku of {
      deku_consensus : address;
      handles_hash: blake2b;
      handle : vault_handle_structure;
      proof : vault_handle_proof;
    }
  | Burn_callback of bytes ticket

type return = operation list * unit

let deposit_to_deku (consensus : address) (ticket : vault_ticket) (recipient : key_hash) =
  let deposit_entrypoint =
    match (Tezos.get_entrypoint_opt "%deposit" consensus : vault_deposit contract option) with
    | Some deposit_entrypoint -> deposit_entrypoint
    | None -> failwith "invalid deku consensus address"
  in
  let deposit_input : vault_deposit = {
    ticket = ticket;
    address = recipient
  } in
  Tezos.transaction deposit_input 0tez deposit_entrypoint

let withdraw_from_deku (consensus : address) (handles_hash : blake2b)
    (handle : vault_handle_structure) (proof : vault_handle_proof)
    (callback : vault_ticket contract) =
  let withdraw_entrypoint =
    match
      (Tezos.get_entrypoint_opt "%withdraw" consensus
        : vault_withdraw contract option)
    with
    | Some withdraw_entrypoint -> withdraw_entrypoint
    | None -> failwith "invalid deku consensus address"
  in
  let withdraw_input = { handles_hash = handles_hash; handle=handle; proof=proof; callback=callback } in
  Tezos.transaction withdraw_input 0tez withdraw_entrypoint

let main ((param, ()) : parameter * unit) : return =
  match param with
  | Mint_to_deku {deku_consensus; deku_recipient; ticket_data; ticket_amount}
    ->
      let ticket = Tezos.create_ticket ticket_data ticket_amount in
      let deposit_operation = deposit_to_deku deku_consensus ticket deku_recipient in
      ([ deposit_operation ], ())
  | Withdraw_from_deku {deku_consensus; handles_hash; handle; proof} ->
      let callback : vault_ticket contract = Tezos.self "%burn_callback" in
      let withdraw_operation =
        withdraw_from_deku deku_consensus handles_hash handle proof callback
      in
      ([ withdraw_operation ], ())
  | Burn_callback _ticket ->
    let operations : operation list = [] in
    (operations, ())
