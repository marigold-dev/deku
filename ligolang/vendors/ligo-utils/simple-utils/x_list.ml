open Base
include List

let rec remove n = function
  | [] -> raise (Failure "List.remove")
  | _  :: tl when n = 0 -> tl
  | hd :: tl -> hd :: remove (n - 1) tl
let set_nth new_i l new_v =
  mapi ~f:(fun old_i old_v -> if old_i = new_i then new_v else old_v) l

let rec remove_element ?compare:cmp x lst =
  let compare = Option.value ~default:Caml.compare cmp in
  match lst with
  | [] -> raise (Failure "List.remove_element")
  | hd :: tl when compare x hd = 0 -> tl
  | hd :: tl -> hd :: remove_element ~compare x tl

let repeat n x = init ~f:(fun _ -> x) n

let to_pair = function
  | [a ; b] -> Some (a, b)
  | _ -> None

let to_singleton = function
  | [a] -> Some a
  | _ -> None

let fold_map2_exn ~f ~init a b =
  fold_map ~f:(fun init (a,b) -> f init a b) ~init @@ zip_exn a b

let rec fold_map_right ~f ~init = function
    | [] -> (init , [])
    | hd :: tl ->
        let init,tl = fold_map_right ~f ~init tl in 
        let init,hd = f init hd in
        init,hd::tl

let uncons = function
  | [] -> None
  | hd :: tl ->
     Some (hd, tl)

module Ne = struct

  type 'a t = 'a * 'a List.t

  let unzip ((hd, tl): _ t) =
    let (a, b) = hd and (la, lb) = List.unzip tl in
    (a, la), (b, lb)
  let of_list lst = List.hd_exn lst, List.tl_exn lst
  let to_list (hd, tl : _ t) = hd :: tl
  let singleton hd : 'a t = hd , []
  let hd : 'a t -> 'a = fst
  let cons : 'a -> 'a t -> 'a t = fun hd' (hd , tl) -> hd' , hd :: tl
  let iter f (hd, tl : _ t) = f hd ; List.iter ~f tl
  let map f (hd, tl : _ t) = f hd, List.map ~f tl
  let fold_left f init (hd, tl : _ t) = List.fold_left ~f ~init:(f init hd) tl
  let hd_map : _ -> 'a t -> 'a t = fun f (hd , tl) -> (f hd , tl)
  let mapi f (hd, tl : _ t) =
    let lst = List.mapi ~f (hd::tl) in
    of_list lst
  let concat (hd, tl : _ t) = hd @ List.concat tl
  let rev (lst : _ t) = of_list @@ List.rev @@ to_list lst
  let find_map = fun f (hd, tl : _ t) ->
    match f hd with
    | Some x -> Some x
    | None -> find_map ~f tl
  let append : 'a t -> 'a t -> 'a t = fun (hd, tl) (hd', tl') ->
    hd, List.append tl @@ hd' :: tl'
  let compare = fun ?compare:cmp (hd, tl) (hd', tl') ->
    let cmp = Option.value ~default:Caml.compare cmp in
    match cmp hd hd' with
      0 -> compare cmp tl tl'
    | c -> c

end
