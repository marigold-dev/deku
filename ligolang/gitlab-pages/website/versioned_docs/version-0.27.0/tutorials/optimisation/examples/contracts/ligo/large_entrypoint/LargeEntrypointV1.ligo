(* Here we have one large entrypoint that gets called rarely,
   and a small entrypoint that gets called often. The computations
   performed by these entrypoints are quite arbitrary, e.g.,
   `large_entrypoint` may be some managerial function that
   checks permissions, computes rewards for stakeholders, etc.
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

(* An entrypoint that occupies a lot of space *)
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

type storage is record [result : int]

function main (const parameter : parameter; const storage : storage) is
block {
  const nop = (list [] : list (operation))
} with
    case parameter of [
      LargeEntrypoint (n) ->
        (nop, storage with record [result = large_entrypoint (n)])
    | SmallEntrypoint (n) ->
        (nop, storage with record [result = small_entrypoint (n)])
    ]
