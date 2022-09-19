open Deku_crypto

type validators = { as_set : Key_hash.Set.t; as_list : Key_hash.t list }
type t = validators

let of_key_hash_list l = { as_set = Key_hash.Set.of_list l; as_list = l }
let to_key_hash_list t = t.as_list
let cardinal validators = Key_hash.Set.cardinal validators.as_set
let mem key_hash validators = Key_hash.Set.mem key_hash validators.as_set

let rec findi_opt n f l =
  match l with
  | [] -> None
  | el :: tl -> if f el then Some n else findi_opt (n + 1) f tl

let findi_opt f l = findi_opt 0 f l

let find_after_index ~after validators =
  findi_opt (fun validator -> Key_hash.equal validator after) validators

let skip ~after ~skip validators =
  let validators = validators.as_list in
  let length = List.length validators in
  match find_after_index ~after validators with
  | Some n ->
      let i = (n + skip) mod length in
      List.nth_opt validators i
  | None -> None
