type parameter = unit

type storage = {
  next_use : timestamp;
  interval : int;
  execute  : unit -> operation list
}

type return = operation list * storage

let main (_, store : parameter * storage) : return =
  (* Multiple evaluations of Tezos.now give different values *)
  let my_now : timestamp = Tezos.now in
  if my_now > store.next_use
  then
    let store : storage =
      {store with next_use = my_now + store.interval}
    in store.execute (), store
  else
    (* TODO: Add the time until next use to this message *)
    (failwith "You have to wait before you can execute this contract again."
     : return)
