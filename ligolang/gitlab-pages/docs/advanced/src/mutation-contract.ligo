// This is testnew.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int

type return is list (operation) * storage

// Two entrypoints
function add (const store : storage; const delta : int) : storage is
  store + delta
function sub (const store : storage; const delta : int) : storage is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  end)


function originate_and_test(const mainf : parameter * storage -> return) is
  block {
    const initial_storage = 5;
    const (taddr, _, _) = Test.originate(mainf, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(7), 1mutez);
    const storage = Test.get_storage(taddr);
    assert (storage = initial_storage + 7);
  } with (unit);

const test = originate_and_test(main);

const test_mutation =
  case Test.mutation_test(main, originate_and_test) of
    None -> unit
  | Some (_, mutation) -> block { Test.log(mutation) }
                          with failwith("Some mutation also passes the tests! ^^")
  end
