(* Here we have one large entrypoint that gets called rarely,
   and a small entrypoint that gets called often. The computations
   performed by these entrypoints are quite arbitrary, e.g.,
   `large_entrypoint` may be some managerial function that
   checks permissions, computes rewards for stakeholders, etc.
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

(* An entrypoint that occupies a lot of space *)
let large_entrypoint (p : int) =
  let x = "A long line of meaningless words occupying storage" in
  let some_lambda =
    fun (n : int) ->
      {
        a = "A large record with dummy values";
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

type storage = { result : int }

let main (parameter, storage : parameter * storage) =
  match parameter with
    LargeEntrypoint n ->
      ([] : operation list), {storage with result = large_entrypoint n}
  | SmallEntrypoint n ->
      ([] : operation list), {storage with result = small_entrypoint n}
