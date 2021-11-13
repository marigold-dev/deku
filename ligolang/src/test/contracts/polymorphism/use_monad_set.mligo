#define USE_SET

#if USE_SET
#import "set_monad.mligo" "M"
#else
#import "list_monad.mligo" "M"
#endif

type t = M.monad

let interval (x : int) (y : int) =
  let rec r ((c, acc) : int * int t) : int t =
    if (c < x) then acc else r (c - 1, M.mplus (M.ret c) acc) in
  r (y, (M.mzero : int t))

let triples (n : int) =
  M.bind (interval 1 n) (fun (x : int) ->
  M.bind (interval 1 n) (fun (y : int) ->
  M.bind (interval 1 n) (fun (z : int) ->
  M.ret (x, y, z))))

let solve (n : int) =
  M.bind (triples n) (fun ((x, y, z) : int * int * int) ->
  if (x * x + y * y = z * z) then
    M.ret (x, y, z)
  else
    (M.mzero : (int * int * int) t))
