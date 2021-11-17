(* Let's see if we can benefit by moving a large entrypoint
   to lazy storage.
*)

(* Some large record for demonstration purposes *)
type large_record is
  record [
    a : string;
    b : int;
    c : bool;
    d : nat;
    e : int -> int;
    f : option (address)
  ]

(* An entrypoint that occupies a lot of space. Now it does not end up
   in the code of the contract: we put it to a big map upon origination;
   see /migrations/5_deploy_large_entrypoint.js *)
function large_entrypoint (const p : int) is
block {
  const x = "A long line of meaningless words occupying storage";
  function some_lambda (const n : int) is
    record [
      a = "A large record with dummy values";
      b = n;
      c = True;
      d = 42;
      e = (function (const t : int) is t + 1);
      f = (None : option (address))
    ];
  const fst = some_lambda (p);
  const snd = some_lambda (p + 2)
} with fst.b + snd.b

function small_entrypoint (const p : int) is p

type parameter is LargeEntrypoint of int | SmallEntrypoint of int

type storage is
  record [large_entrypoint : big_map (bool, int -> int); result : int]

function load_large_ep (const storage : storage) is
block {
  const maybe_large_entrypoint : option (int -> int)
  = Map.find_opt (True, storage.large_entrypoint)
} with
    case maybe_large_entrypoint of [
      Some (ep) -> ep
    | None -> (failwith ("Internal error") : int -> int)
    ]

function main (const parameter : parameter; const storage : storage) is
block {
  const nop = (list [] : list (operation))
} with
    case parameter of [
      LargeEntrypoint (n) ->
        (nop, storage with record [result = (load_large_ep (storage)) (n)])
    | SmallEntrypoint (n) ->
        (nop, storage with record [result = small_entrypoint (n)])
    ]
