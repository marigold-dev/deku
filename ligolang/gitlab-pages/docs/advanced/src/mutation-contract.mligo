// This is mutation-contract.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n))

let originate_and_test (mainf : parameter * storage -> return) =
  let initial_storage = 7 in
  let (taddr, _, _) = Test.originate mainf initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn contr (Increment (7)) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 7)

let test = originate_and_test main

let test_mutation =
  match Test.mutation_test main originate_and_test with
    None -> ()
  | Some (_, mutation) -> let () = Test.log(mutation) in
                          failwith "Some mutation also passes the tests! ^^"
