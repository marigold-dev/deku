type foobar = (int, int) map

let empty_map : foobar = Map.empty

let map1 : foobar =
  Map.literal [(144,23); (51,23); (42,23); (120,23); (421,23)]

let map2 : foobar = Map.literal [(23,0); (42,0)]

let set_2 (n : int) (m : foobar) : foobar = Map.update 23 (Some n) m

let set_ (t : int * foobar) : foobar = set_2 t.0 t.1

let add (n,m : int * foobar) : foobar = Map.add 23 n m

let rm (m : foobar) : foobar = Map.remove 42 m

(* Dummy test so that we can add the same test for PascaLIGO *)

let patch_ (_m : foobar) : foobar = Map.literal [(0,5); (1,6); (2,7)]

(* Second dummy test, see above *)

let patch_empty (_m : foobar) : foobar = Map.literal [(0,0); (1,1); (2,2)]

(* Third dummy test, see above *)

let patch_deep (_m : foobar * nat) : foobar * nat =
  Map.literal [(0,0); (1,9); (2,2)], 10n

let size_ (m : foobar) : nat = Map.size m

let get  (m : foobar) : int option = Map.find_opt 42 m
let get_ (m : foobar) : int option = Map.find_opt 42 m

let mem (k,m : int * foobar) : bool = Map.mem k m

let iter_op (m : foobar) : unit =
  let assert_eq = fun (a, b : int * int) -> assert (a = b)
  in Map.iter assert_eq m

let map_op (m : foobar) : foobar =
  let increment = fun (i : int * int) -> i.1 + 1
  in Map.map increment m

let fold_op (m : foobar) : int =
  let aggregate = fun (i, m : int * (int * int)) -> i + m.0 + m.1
  in Map.fold aggregate m 10

let deep_op (m: foobar) : foobar =
  let coco = 0, m in
  let coco = 0, Map.remove 42 coco.1 in
  let coco = 0, Map.update 32 (Some 16) coco.1
  in coco.1
