open Deku_crypto

type validators = Key_hash.t list
and t = validators [@@deriving yojson]

let of_key_hash_list t = t
let to_key_hash_list t = t

let cardinal validators =
  (* TODO: O(1) cardinality *)
  List.length validators

let mem key_hash validators = List.mem key_hash validators

let rec findi_opt n f l =
  match l with
  | [] -> None
  | el :: tl -> if f el then Some n else findi_opt (n + 1) f tl

let findi_opt f l = findi_opt 0 f l

let find_after_index ~after validators =
  findi_opt (fun validator -> Key_hash.equal validator after) validators

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

let skip ~after ~skip validators =
  let length = List.length validators in
  let n =
    match find_after_index ~after validators with Some n -> n | None -> 0
  in
  (* FIXME: how could this be negative? *)
  let i = modulo (n + skip) length in
  List.nth validators i
