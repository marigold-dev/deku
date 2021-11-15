type storage = int * int list

type parameter = int list

type return = operation list * storage

let x : int list = []
let y : int list = [3; 4; 5]
let z : int list = 2::y

let main (p, s: parameter * storage) : return =
  let storage =
    match p with
          [] -> s
    | hd::tl -> s.0 + hd, tl
  in ([] : operation list), storage

let size_ (s : int list) : nat = List.length s

let fold_op (s : int list) : int =
  let aggregate = fun (t : int * int) -> t.0 + t.1
  in List.fold aggregate s 10

let fold_left (s : int list) : int list =
  let aggregate = fun (t : int list * int) -> t.1 :: t.0 
  in List.fold_left aggregate ([] : int list) s

let fold_right (s : int list) : int list =
  let aggregate = fun (t : int * int list) -> t.0 :: t.1
  in List.fold_right aggregate s ([] : int list)

let map_op (s : int list) : int list =
  List.map (fun (cur : int) -> cur + 1) s

let iter_op (s : int list) : unit =
  let do_nothing = fun (_ : int) -> unit
  in List.iter do_nothing s
