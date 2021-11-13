let main (_, _ : unit * unit) : operation list * unit =
    let (op, _) = Tezos.create_contract (fun ((k,v), s : (int * nat) * (int, nat) big_map) : operation list * (int, nat) big_map ->
        ([] : operation list), Big_map.add k v s) (None : key_hash option) 0tez (Big_map.empty : (int, nat) big_map) in
    ([ op ; ], ())

let test_main =
    let (ta, _, _) =  Test.originate main () 0tez in
    let c : unit contract = Test.to_contract ta in
    Test.transfer_to_contract_exn c () 0tez
