(* Unit testing of module [PolyMap] *)

let () = Printf.printf "Testing polymorphic maps... "

let cmp = Pervasives.compare

let in_assoc =
  ["c", 2;
   "g", -1;
   "a", 0;
   "e", 4;
   "f", 5;
   "g", -1;
   "b", -1;
   "b", 1;
   "d", 3;
   "j", 9;
   "g", 6;
   "h", 7;
   "i", 8]

(* Insertion sort which keeps the last duplicate of any item.
   WARNING: Quadratic cost! *)

let rec ins cmp x = function
         [] -> [x]
| y::s as l ->
    let diff = cmp x y in
    if diff = 0 then l else if diff > 0 then y :: ins cmp x s else x::l

let rec isort cmp = function
    [] -> []
| x::s -> ins cmp x (isort cmp s)


let sorted_in_assoc = isort (fun (k1,_) (k2,_) -> cmp k1 k2) in_assoc

(*
let () = Printf.printf "\nInput map:\n"
let () = List.iter (fun (k,v) -> Printf.printf "%s -> %d\n" k v) in_assoc
let () = Printf.printf "\nSorted map:\n"
let () = List.iter (fun (k,v) -> Printf.printf "%s -> %d\n" k v) sorted_in_assoc
*)

let empty_map = PolyMap.create ~cmp
let out_map =
  List.fold_left (fun m (k,v) -> PolyMap.add k v m) empty_map in_assoc

let out_bindings = PolyMap.bindings out_map

(*
let () = Printf.printf "\nOutput map:\n"
let () = List.iter (fun (k,v) -> Printf.printf "%s -> %d\n" k v) out_bindings
*)

let () =
  if sorted_in_assoc = out_bindings then
    Printf.printf "PASS.\n%!"
  else Printf.printf "FAILED.\n%!"
