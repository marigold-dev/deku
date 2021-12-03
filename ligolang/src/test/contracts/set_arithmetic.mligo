(* Test set operations in CameLIGO *)

let literal_op ((): unit) : string set =
  Set.literal ["foo"; "bar"; "foobar"]

let size_op (s: string set) : nat = Set.cardinal s

let add_op (s : string set) : string set =
   Set.add "foobar" s

let remove_op (s : string set) : string set =
   Set.remove "foobar" s

let remove_deep (s : string set * nat) : string set * nat =
  (Set.remove "foobar" s.0, s.1)

(* homogeneity with pascaligo *)
let patch_op (s: string set) : string set =
  Set.add "foobar" s

let patch_op_deep (s: string set * nat) : string set * nat =
  (Set.add "foobar" s.0, s.1)


let mem_op (s : string set) : bool = Set.mem "foobar" s


let upd (s: string set) (flag: bool) : string set = Set.update "foobar" flag s 

let iter_op (s : int set) : int =
  let () = Set.iter (fun (_ : int) -> ()) s in
  0

let aggregate ( (i,j) : int list * int) : int list = j :: i

let fold_op (s : int set) : int list = Set.fold aggregate s ([] : int list)

let aggregate ( (i,j) : int * int list) : int list = i :: j
let fold_right (s : int set ) : int list = Set.fold_desc aggregate s ([] : int list)