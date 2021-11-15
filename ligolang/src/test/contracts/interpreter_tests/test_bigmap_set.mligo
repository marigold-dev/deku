
type storage = (int, nat -> nat) big_map

let main ((k,v), s : (int * (nat -> nat)) * storage) : operation list * storage =
  ([] : operation list), Big_map.add k v s

let test =
  let init = (Big_map.empty : storage) in
  let (taddr, _, _) = Test.originate main init 0tez in
  let ctr = Test.to_contract taddr in
  let y : nat = 1n in
  let () = Test.transfer_to_contract_exn ctr (21, (fun (x : nat) -> x * 2n + y)) 0tez in
  let y : nat = 100n in
  let init = Big_map.add 21 (fun (x : nat) -> 0n) (Big_map.empty : (int, nat -> nat) big_map) in
  let () = Test.set_big_map 5 init in
  let m_new = Test.get_storage taddr in
  let v = Big_map.find_opt 21 m_new in
  match v with
  | Some f ->
      let () = Test.log (f 4n) in
      let () = Test.set_big_map 4 init in
      let m_new = Test.get_storage taddr in
      let v = Big_map.find_opt 21 m_new in
      (match v with
       | Some f ->
           Test.log (f 4n)
       | None ->
           Test.log "Error")
  | None ->
      Test.log "Error"
