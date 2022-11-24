open Deku_crypto

type validators = { set : Key_hash.Set.t; list : Key_hash.t list }
and t = validators

let of_key_hash_list validators =
  match validators with
  | [] -> raise (Invalid_argument "validators cannot be empty")
  | validators ->
      let set = Key_hash.Set.of_list validators in
      assert (Key_hash.Set.cardinal set = List.length validators);
      { set; list = validators }

let to_key_hash_list validators = validators.list

let cardinal validators =
  (* TODO: O(1) cardinality *)
  List.length validators.list

let mem key_hash validators = Key_hash.Set.mem key_hash validators.set

let rec findi_opt n f l =
  match l with
  | [] -> None
  | el :: tl -> if f el then Some n else findi_opt (n + 1) f tl

let findi_opt f l = findi_opt 0 f l

let find_after_index ~after validators =
  findi_opt (fun validator -> Key_hash.equal validator after) validators

let skip ~after ~skip validators =
  let validators = validators.list in
  let length = List.length validators in
  let index =
    match find_after_index ~after validators with
    | Some index -> index
    | None -> 0
  in
  let index = (index + skip) mod length in
  let index =
    match index < 0 with
    | true ->
        Logs.warn (fun m -> m "negative skip: %d" skip);
        0
    | false -> index
  in
  List.nth validators index

let encodings =
  let open Data_encoding in
  conv to_key_hash_list of_key_hash_list (list Key_hash.encoding)
