type storage = bytes
type parameter = chest_key * chest

type return = operation list * storage

let main (p , _ : parameter * storage) : return =
  let (ck,c) = p in
  let new_s =
    match Tezos.open_chest ck c 1000n with
    | Ok_opening b -> b
    | Fail_decrypt -> 0x01
    | Fail_timelock -> 0x00
  in
  (([] : operation list), new_s)
