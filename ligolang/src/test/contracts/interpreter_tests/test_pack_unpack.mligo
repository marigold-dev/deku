let main ((b, _) : bytes * bytes) : operation list * bytes =
  ([] : operation list), b

let test =
  let b = Bytes.pack 42n in
  let (ta, _, _) = Test.originate main b 0tez in
  let () = assert ((Bytes.unpack (Test.get_storage ta) : nat option) = Some 42n) in
  let c = Test.to_contract ta in
  let b = Bytes.pack "bonjour" in
  let () = Test.transfer_to_contract_exn c b 0tez in
  assert ((Bytes.unpack (Test.get_storage ta) : string option) = Some "bonjour")
