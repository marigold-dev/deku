type storage = bytes
type parameter = chest_key * chest

type return = operation list * storage

let main (p , _ : parameter * storage) : return =
  let (ck,c) = p in
  let new_s =
    match Tezos.open_chest ck c 10n with
    | Ok_opening b -> b
    | Fail_timelock -> 0x00
    | Fail_decrypt -> 0x01
  in
  (([] : operation list), new_s)


let test =
  let init_storage : bytes = 0x00 in
  let (addr,_,_) = Test.originate main init_storage 0tez in
  let payload = 0x0101 in

  let test_open (cc : chest_key * chest) (expected : bytes) : unit =
    let x : parameter contract = Test.to_contract addr in
    let () = Test.transfer_to_contract_exn x cc 0tez in
    let s = Test.get_storage addr in
    assert (s = expected)
  in

  let test1 = (* chest key/payload and time matches -> OK *)
    let (chest,chest_key) = Test.create_chest payload 10n in
    test_open (chest_key,chest) payload
  in
  let test2 = (* chest key/payload do not match -> Fail_decrypt *)
    let (chest,_) = Test.create_chest payload 10n in
    let (_,chest_key) = Test.create_chest 0x2020 10n in
    test_open (chest_key,chest) 0x01
  in
  let test3 = (* chest time do not match -> Fail_timelock *)
    let (chest,_) = Test.create_chest payload 2n in
    let chest_key = Test.create_chest_key chest 10n in
    test_open (chest_key,chest) 0x00
  in
  ()
