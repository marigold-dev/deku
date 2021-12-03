(* Let's see if we can benefit by moving a large entrypoint
   to lazy storage.
*)

(* Some large record for demonstration purposes *)
type large_record = {
  a : string;
  b : int;
  c : bool;
  d : nat;
  e : (int -> int);
  f : address option
}

(* An entrypoint that occupies a lot of space. Now it does not end up
   in the code of the contract: we put it to a big map upon origination;
   see /migrations/5_deploy_large_entrypoint.js *)
let large_entrypoint (p : int) =
  let x = "A long line of meaningless words occupying storage" in
  let some_lambda =
    fun (n : int) ->
      {
        a = "Large record with dummy values";
        b = n;
        c = true;
        d = 42;
        e = fun (t : int) -> t + 1;
        f = (None : address option)
      } in
  let fst = some_lambda p in
  let snd = some_lambda (p + 2) in
  fst.b + snd.b

let small_entrypoint (p : int) = p

type parameter = LargeEntrypoint of int | SmallEntrypoint of int

type storage = { large_entrypoint : (bool, int -> int) big_map; result : int }

let load_large_ep (storage : storage) =
  let maybe_large_entrypoint : (int -> int) option =
    Big_map.find_opt true (storage.large_entrypoint) in
  match maybe_large_entrypoint with
    Some ep -> ep
  | None -> (failwith "Internal error" : (int -> int))

let main (parameter, storage : parameter * storage) =
  match parameter with
    LargeEntrypoint n ->
      ([] : operation list), {storage with result = (load_large_ep storage) n}
  | SmallEntrypoint n ->
      ([] : operation list), {storage with result = small_entrypoint n}
