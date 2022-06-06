open Helpers
open Crypto

exception Invalid_json of string

module Validators_action = struct
  type t =
    | Add_validator    of Validators.validator
    | Remove_validator of Validators.validator
  [@@deriving eq, ord, yojson]

  let hash payload = to_yojson payload |> Yojson.Safe.to_string |> BLAKE2B.hash

  let sign secret t =
    let hash = hash t in
    Signature.sign secret hash

  let verify key signature t =
    let hash = hash t in
    Signature.verify key signature hash
end

(*********************************************************************)
(* UTILS                                                             *)
(*********************************************************************)

let yojson_of_set m =
  match m with
  | Some s ->
    Trusted_validators_membership_change.Set.elements s
    |> [%to_yojson: Trusted_validators_membership_change.t list]
  | None -> `Null

let set_of_yojson s =
  match s with
  | `List l ->
    let list = List.map Trusted_validators_membership_change.of_yojson l in
    let b = List.exists (fun m -> m = Error "fail") list in
    if b then
      Result.Error "Fail"
    else
      let values = List.map (fun m -> Result.get_ok m) list in
      Result.Ok (Some (Trusted_validators_membership_change.Set.of_list values))
  | _ -> Result.Error "Fail"

let string_to_key : string -> Crypto.Key_hash.t =
 fun my_str ->
  let key_opt = Crypto.Key_hash.of_string my_str in
  match key_opt with
  | None -> failwith "Couldn't retrive key from string!"
  | Some key -> key

(*********************************************************************)
(* TYPE                                                               *)
(*********************************************************************)

type t = {
  validators : Validators.t;
  trusted_change : Trusted_validators_membership_change.Set.t option;
      [@of_yojson set_of_yojson] [@to_yojson yojson_of_set]
  (* TODO: use proper Timestamp *)
  last_seen_membership_change_timestamp : float;
}
[@@deriving to_yojson, of_yojson]

let empty =
  {
    validators = Validators.empty;
    trusted_change = None;
    last_seen_membership_change_timestamp = 0.0;
  }

(*********************************************************************)
(* VALIDATORS                                                        *)
(*********************************************************************)

let get_validators : t -> Validators.t = fun t -> t.validators

let update_validators : Validators.validator list -> t -> t =
 fun validators t ->
  let new_validators =
    List.fold_left (fun s v -> Validators.add v s) (get_validators t) validators
  in

  { t with validators = new_validators }

let add_validator : Crypto.Key_hash.t -> t -> t =
 fun validator t ->
  let validator : Validators.validator = { address = validator } in
  let new_validators = Validators.add validator (get_validators t) in
  { t with validators = new_validators }

let remove_validator : Crypto.Key_hash.t -> t -> t =
 fun validator t ->
  let validator : Validators.validator = { address = validator } in
  let new_validators = Validators.remove validator (get_validators t) in
  { t with validators = new_validators }

(*********************************************************************)
(* MEMBERSHIP/TRUSTED                                                *)
(*********************************************************************)

let get_trusted_change_opt :
    t -> Trusted_validators_membership_change.Set.t option =
 fun t -> t.trusted_change

let set_trusted_change : Trusted_validators_membership_change.Set.t -> t -> t =
 fun trusted_change t -> { t with trusted_change = Some trusted_change }

let is_add_validator_operation_trusted : Crypto.Key_hash.t -> t -> bool =
 fun address t ->
  let trusted_opt = get_trusted_change_opt t in
  match trusted_opt with
  | None -> false
  | Some trusted ->
    Trusted_validators_membership_change.Set.mem { address; action = Add }
      trusted

let is_remove_validator_operation_trusted : Crypto.Key_hash.t -> t -> bool =
 fun address t ->
  let trusted_opt = get_trusted_change_opt t in
  match trusted_opt with
  | None -> false
  | Some trusted ->
    Trusted_validators_membership_change.Set.mem
      { address; action = Remove }
      trusted

let is_operation_trusted : Validators_action.t -> t -> bool =
 fun op t ->
  match op with
  | Add_validator validator ->
    is_add_validator_operation_trusted validator.address t
  | Remove_validator validator ->
    is_remove_validator_operation_trusted validator.address t

let delete_remove_validator_operation : Crypto.Key_hash.t -> t -> t =
 fun address t ->
  let trusted_opt = get_trusted_change_opt t in
  match trusted_opt with
  | None -> failwith "Trusted change is None, you moron"
  | Some trusted ->
    let trusted =
      Trusted_validators_membership_change.Set.remove
        { address; action = Remove }
        trusted in
    { t with trusted_change = Some trusted }

let delete_add_validator_operation : Crypto.Key_hash.t -> t -> t =
 fun address t ->
  let trusted_opt = get_trusted_change_opt t in
  match trusted_opt with
  | None -> failwith "Trusted change is None, you moron"
  | Some trusted ->
    let trusted =
      Trusted_validators_membership_change.Set.remove { address; action = Add }
        trusted in
    { t with trusted_change = Some trusted }

let clean_operations : Validators_action.t -> t -> t =
 fun op t ->
  match op with
  | Add_validator validator ->
    delete_add_validator_operation validator.address t
  | Remove_validator validator ->
    delete_remove_validator_operation validator.address t

let add_add_validator_operation : Crypto.Key_hash.t -> t -> t =
 fun address t ->
  let trusted_opt = get_trusted_change_opt t in
  match trusted_opt with
  | None -> failwith "Trusted change is None, you moron"
  | Some trusted ->
    let trusted =
      Trusted_validators_membership_change.Set.add { address; action = Add }
        trusted in
    { t with trusted_change = Some trusted }

let add_remove_validator_operation : Crypto.Key_hash.t -> t -> t =
 fun address t ->
  let trusted_opt = get_trusted_change_opt t in
  match trusted_opt with
  | None -> failwith "Trusted change is None, you moron"
  | Some trusted ->
    let trusted =
      Trusted_validators_membership_change.Set.add
        { address; action = Remove }
        trusted in
    { t with trusted_change = Some trusted }

(*********************************************************************)
(* LAST CHANGE TIMESTAMP                                             *)
(*********************************************************************)

let get_last_change_timestamp : t -> float =
 fun t -> t.last_seen_membership_change_timestamp

let update_last_change_timestamp : float -> t -> t =
 fun timestamp t -> { t with last_seen_membership_change_timestamp = timestamp }

(*********************************************************************)
(* BLOCK PROPOSER                                                    *)
(*********************************************************************)

let get_new_block_proposer : int -> t -> Validators.validator option =
 fun skips t -> Validators.after_current skips t.validators

let update_block_proposer : Crypto.Key_hash.t -> t -> t =
 fun block_author t ->
  (* TODO: check is sender ok *)
  { t with validators = Validators.update_current block_author t.validators }

(*********************************************************************)
(* VALIDATORS (list, number, etc)                                    *)
(*********************************************************************)
let get_number_of_validators : t -> int =
 fun t -> Validators.length t.validators

let get_validators_hash : t -> Crypto.BLAKE2B.t =
 fun t -> Validators.hash t.validators

let get_validators_list : t -> Validators.validator list =
 fun t -> Validators.to_list t.validators

let get_validators_address_list : t -> Crypto.Key_hash.t list =
 fun t ->
  let validators_list = get_validators_list t in
  List.map (fun v -> v.Validators.address) validators_list

(*********************************************************************)
(* VALIDATORS (modifying)                                            *)
(*********************************************************************)

let process_add_validator : Validators.validator -> t -> t =
 fun new_validator t ->
  let new_validator = new_validator.address in
  (* Add in t.validators *)
  let t = add_validator new_validator t in
  (* Add in t.trusted_change *)
  let t = add_add_validator_operation new_validator t in
  (* Change timestamp *)
  let new_time = Unix.time () in
  { t with last_seen_membership_change_timestamp = new_time }

let process_remove_validator : Validators.validator -> t -> t =
 fun old_validator t ->
  let old_validator = old_validator.address in
  (* Remove in t.validators *)
  let t = remove_validator old_validator t in
  (* Remove in t.trusted_change *)
  let t = add_remove_validator_operation old_validator t in
  (* Change timestamp *)
  let new_time = Unix.time () in
  { t with last_seen_membership_change_timestamp = new_time }

(*********************************************************************)
(* MANDATORY                                                         *)
(*********************************************************************)

let tick _timestamp t = (t, [])

let process_operation : Validators_action.t -> t -> t =
 fun op t ->
  match op with
  | Add_validator new_validator -> process_add_validator new_validator t
  | Remove_validator old_validator -> process_remove_validator old_validator t

(*********************************************************************)
(* DUMP/LOAD/PERSIST                                                 *)
(*********************************************************************)

(* write_json and read_json stolen from files.ml due to dep. TODO: refacto. *)
let write_json to_yojson data ~file =
  Lwt_io.with_file ~mode:Output file (fun oc ->
      Lwt_io.write oc (Yojson.Safe.pretty_to_string (to_yojson data)))

let read_json of_yojson ~file =
  let%await my_string = Lwt_io.with_file ~mode:Input file Lwt_io.read in
  let json = Yojson.Safe.from_string my_string in
  match of_yojson json with
  | Ok data -> await data
  | Error error -> raise (Invalid_json error)

(* Should it be replaced by: *)
(* write_json yojson_of_set t.trusted_change ~file:t.file + add t.file *)
let dump : t -> unit Lwt.t =
 fun _t -> failwith "Not implemented yet because not needed yet"

(* NOTE: load still uses trusted validators membership change to minimize changes in deku in this PR. *)
let load : file:string -> t Lwt.t =
 fun ~file ->
  let%lwt trusted_change_list =
    read_json [%of_yojson: Trusted_validators_membership_change.t list] ~file
  in

  let prenode = empty in
  let trusted_change_set =
    Trusted_validators_membership_change.Set.of_list trusted_change_list in

  let prenode = set_trusted_change trusted_change_set prenode in

  let prenode =
    List.fold_left
      (fun prenode (elem : Trusted_validators_membership_change.t) ->
        match elem.action with
        | Add -> add_validator elem.address prenode
        | Remove -> remove_validator elem.address prenode)
      prenode trusted_change_list in
  Lwt.return prenode