(* Unit testing of module [RedBlack] *)

open RedBlack;;

let () = Printf.printf "Testing Red-black trees... "

let cmp = Pervasives.compare

let in_items = [6;7;9;1;0;3;6;1;8;5;4;2]
let sorted_in_items = List.sort_uniq cmp in_items

let () = Printf.printf "\nInput items:  "
let () = List.iter (fun x -> Printf.printf "%d " x) in_items
let () = Printf.printf "\nSorted input: "
let () = List.iter (fun x -> Printf.printf "%d " x) sorted_in_items

let t = List.fold_right (add ~cmp Old) in_items empty
let out_items = elements t

let () = Printf.printf "\nOutput items: "
let () = List.iter (fun x -> Printf.printf "%d " x) out_items
let () = Printf.printf "\n%!"

let () =
  if sorted_in_items = out_items then
    Printf.printf "PASS.\n%!"
  else Printf.printf "FAILED.\n%!"

let rec draw ~pad:(pd,pc) = function
  Ext -> ()
| Int (colour, left, root, right) ->
    let app i sub =
      let pad =
        (pc ^ (if i = 1 then "`-- " else "|-- "),
         pc ^ (if i = 1 then "    " else "|   "))
      in draw ~pad sub in
    begin
      Printf.printf "%s%s(%d)\n" pd (if colour = Red then "R" else "B") root;
      List.iteri app [left; right]
    end

let () = draw ~pad:("","") t

let rec to_string buffer ~pad:(pd,pc) = function
  Ext -> ()
| Int (colour, left, root, right) ->
    let root_str =
      Printf.sprintf "%s%s(%d)\n" pd
        (if colour = Red then "R" else "B") root in
    let app rank sub =
      let pad =
        (pc ^ (if rank = 1 then "`-- " else "|-- "),
         pc ^ (if rank = 1 then "    " else "|   "))
      in to_string buffer ~pad sub in
    begin
      Buffer.add_string buffer root_str;
      List.iteri app [left; right]
    end

let to_string tree =
  let buffer = Buffer.create 131 in
  let     () = to_string buffer ~pad:("","") tree
  in Buffer.contents buffer

let () = to_string t |> print_string |> print_newline

let rec pretty buffer ~pad:(pd,pc) = function
  Ext -> Buffer.add_string buffer (pd ^ "Ext\n")
| Int (colour, left, root, right) ->
    let root_str =
      Printf.sprintf "%sInt (%s,%d)\n" pd
        (if colour = Red then "Red" else "Black") root in
    let app rank sub =
      let pad =
        pc ^ (if rank = 0 then "|-- " else "`-- "),
        pc ^ (if rank = 0 then "|   " else "    ")
      in pretty buffer ~pad sub in
    begin
      Buffer.add_string buffer root_str;
      List.iteri app [left; right]
    end

let pretty tree =
  let buffer = Buffer.create 131 in
  let     () = pretty buffer ~pad:("","") tree
  in Buffer.contents buffer

let () = pretty t |> print_string |> print_newline
