// This is mutation-contract.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int);

type return = (list (operation), storage);

// Two entrypoints
let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))}))
};

let originate_and_test = (mainf : (parameter, storage) => return) => {
  let initial_storage = 5;
  let (taddr, _, _) = Test.originate(mainf, initial_storage, 0tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (7)), 1mutez);
  assert (Test.get_storage(taddr) == initial_storage + 7)
};

let test = originate_and_test(main);

let test_mutation =
  switch(Test.mutation_test_all(main, originate_and_test)) {
  | [] => ()
  | ms => { List.iter ((((_, mutation) : (unit, mutation)) => {
                        let path = Test.save_mutation(".", mutation);
                        Test.log("saved at:");
                        Test.log(path);}), ms);
            failwith ("Some mutation also passes the tests! ^^") }
  };
