type storage = bytes ticket list
type return = operation list * storage

type vault_ticket = bytes ticket
type vault_deposit = {
  ticket: vault_ticket;
  address: address
}

type parameter =
| Deposit of {
  deku_consensus : address;
  deku_recipient : address;
  bytes : bytes;
}
| Withdraw of bytes ticket

let main (param, stored_tickets : parameter * storage) : return =
 match param with
 | Deposit {deku_consensus; deku_recipient; bytes} ->
   (match (Tezos.get_entrypoint_opt "%deposit" deku_consensus : vault_deposit contract option)  with 
   | Some contract ->
    (* Content of ticket is just a packed int *)
    let ticket = Tezos.create_ticket bytes 1000000n in 
    let param  = { ticket = ticket; address = deku_recipient } in
    let op = Tezos.transaction param 0mutez contract in
    (* We increment the counter to add some variety to the data *)
    [op], stored_tickets
   | None -> failwith "Entrypoint doesn't exist")
  | Withdraw ticket ->
    let ops : operation list = [] in
    ops , ticket :: stored_tickets