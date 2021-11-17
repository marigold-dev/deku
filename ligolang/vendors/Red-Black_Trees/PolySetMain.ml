(* Unit testing of module [PolySet] *)

open PolySet;;

let () = Printf.printf "Testing polymorphic sets... "

let cmp = Pervasives.compare

let in_items = [6;7;9;1;0;3;6;1;8;5;4;2]
let sorted_in_items = List.sort_uniq cmp in_items

(*
let () = Printf.printf "\nInput items:  "
let () = List.iter (fun x -> Printf.printf "%d " x) in_items
let () = Printf.printf "\nSorted input: "
let () = List.iter (fun x -> Printf.printf "%d " x) sorted_in_items
*)

let empty_set = create ~cmp

let set = List.fold_right add in_items empty_set
let out_items = elements set

(*
let () = Printf.printf "\nOutput items: "
let () = List.iter (fun x -> Printf.printf "%d " x) out_items
let () = Printf.printf "\n%!"
*)

let () =
  if sorted_in_items = out_items then
    Printf.printf "PASS.\n%!"
  else Printf.printf "FAILED.\n%!"

