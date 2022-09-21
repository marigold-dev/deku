open Deku_crypto

type validators = Key_hash.Set.t
and t = validators [@@deriving yojson]

let of_key_hash_list = Key_hash.Set.of_list

let cardinal validators =
  (* TODO: O(1) cardinality *)
  Key_hash.Set.cardinal validators

let mem key_hash validators = Key_hash.Set.mem key_hash validators

let rec findi_opt n f l =
  match l with
  | [] -> None
  | el :: tl -> if f el then Some n else findi_opt (n + 1) f tl

let findi_opt f l = findi_opt 0 f l

let find_after_index ~after validators =
  findi_opt (fun validator -> Key_hash.equal validator after) validators

let skip ~after ~skip validators =
  let validators = Key_hash.Set.elements validators in
  let length = List.length validators in
  match find_after_index ~after validators with
  | Some n ->
      let i = (n + skip) mod length in
      List.nth_opt validators i
  | None -> None
