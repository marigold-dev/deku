
type storage = (int, nat) big_map

let main ((k,v), s : (int * nat) * storage) : operation list * storage =
  ([] : operation list), Big_map.add k v s

let test =
  let init = Big_map.add 12 42n (Big_map.empty : storage) in
  let (taddr, _, _) = Test.originate main init 0tez in
  let init = Big_map.add 32 42n (Big_map.empty : storage) in
  let (taddr, _, _) = Test.originate main init 0tez in
  let ctr = Test.to_contract taddr in
  let m_old = Test.get_storage taddr in
  let () = Test.log m_old in
  let () = Test.log (Big_map.find_opt 21 m_old) in
  let () = Test.transfer_to_contract_exn ctr (21, 42n) 0tez in
  let () = Test.transfer_to_contract_exn ctr (3, 42n) 0tez in
  let m_new = Test.get_storage taddr in
  let () = Test.log m_old in
  let () = Test.log m_new in
  let () = Test.log (Big_map.find_opt 21 m_old) in
  let () = Test.log (Big_map.find_opt 21 m_new) in
  ()
