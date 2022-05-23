open Watcher
open Helpers

module type VALIDATORS_PARAMETER = sig
  module Threshold : sig
    val get : Helpers.Height.t -> int
  end
end

module Raw (P : VALIDATORS_PARAMETER) = struct
  module Action = struct
    open Bin_prot.Std
    type t =
      | Add_validator    of string
      | Remove_validator of string
    [@@deriving ord, bin_io, yojson]
  end

  (*********************************************************************)
  (* UTILS                                                             *)
  (*********************************************************************)

  let yojson_of_set m =
    match m with
    | Some s ->
      Validator_internals.Trusted_validators_membership_change.Set.elements s
      |> [%to_yojson:
           Validator_internals.Trusted_validators_membership_change.t list]
    | None -> `Null

  let set_of_yojson s =
    match s with
    | `List l ->
      let list =
        List.map
          Validator_internals.Trusted_validators_membership_change.of_yojson l
      in
      let b = List.exists (fun m -> m = Error "fail") list in
      if b then
        Result.Error "Fail"
      else
        let values = List.map (fun m -> Result.get_ok m) list in
        Result.Ok
          (Some
             (Validator_internals.Trusted_validators_membership_change.Set
              .of_list values))
    | _ -> Result.Error "Fail"

  let string_to_key : string -> Crypto.Key_hash.t =
   fun my_str ->
    let key_opt = Crypto.Key_hash.of_string my_str in
    match key_opt with
    | None -> failwith "Couldn't retrive key from string!"
    | Some key -> key

  (*********************************************************************)
  (* TYPE             F                                                 *)
  (*********************************************************************)

  type t = {
    validators : Validator_internals.Validators.t;
    membership :
      Validator_internals.Trusted_validators_membership_change.t option;
    trusted_change :
      Validator_internals.Trusted_validators_membership_change.Set.t option;
        [@of_yojson set_of_yojson] [@to_yojson yojson_of_set]
    persist_trusted_change :
      Validator_internals.Trusted_validators_membership_change.t list option;
    (* ->
       unit Lwt.t *)
    last_seen_membership_change_timestamp : float;
        (* TODO: use proper Timestamp *)
  }
  [@@deriving to_yojson, of_yojson]

  let empty =
    {
      validators = Validator_internals.Validators.empty;
      membership = None;
      trusted_change = None;
      persist_trusted_change = None;
      last_seen_membership_change_timestamp = 0.0;
    }

  (*********************************************************************)
  (* MEMBERSHIP/TRUST                                                  *)
  (*********************************************************************)

  let get_membership :
      t -> Validator_internals.Trusted_validators_membership_change.t option =
   fun t -> t.membership

  let update_membership :
      Validator_internals.Trusted_validators_membership_change.t -> t -> t =
   fun membership t -> { t with membership = Some membership }

  let get_trusted_change_opt :
      t -> Validator_internals.Trusted_validators_membership_change.Set.t option
      =
   fun t -> t.trusted_change

  let set_trusted_change :
      Validator_internals.Trusted_validators_membership_change.Set.t -> t -> t =
   fun trusted_change t -> { t with trusted_change = Some trusted_change }

  let is_validator_already_registered : Message.t -> t -> bool =
   fun msg t ->
    PseudoEffect.returner @@ fun { return } ->
    let deku_op = Message.get_sub_category_opt msg in

    let deku_op =
      match deku_op with
      | None -> return false
      | Some deku_op -> deku_op in

    assert (deku_op.operation_family = Deku_operation.Validators);

    let payload = Message.get_payload msg in
    let validator_string =
      Pollinate.Util.Encoding.unpack Bin_prot.Std.bin_read_string payload in
    let validator_address_opt = Crypto.Key_hash.of_string validator_string in

    let validator_address =
      match validator_address_opt with
      | None -> return false
      | Some validator_address -> validator_address in

    let trusted_change =
      match t.trusted_change with
      | None -> return false
      | Some trusted_change -> trusted_change in

    match deku_op.operation_detail with
    | Validators_operation Add_validator ->
      Validator_internals.Trusted_validators_membership_change.Set.mem
        { address = validator_address; action = Add }
        trusted_change
    | Validators_operation Remove_validator ->
      Validator_internals.Trusted_validators_membership_change.Set.mem
        { address = validator_address; action = Remove }
        trusted_change
    | _ -> return false

  let update_trusted : Action.t -> t -> t =
   fun action t ->
    let trusted_change = get_trusted_change_opt t in
    match trusted_change with
    | None -> failwith "Trusted change is None, you moron"
    | Some x ->
      let address, action =
        match action with
        | Add_validator validator ->
          ( Crypto.Key_hash.of_string validator,
            Validator_internals.Trusted_validators_membership_change.Add )
        | Remove_validator validator ->
          ( Crypto.Key_hash.of_string validator,
            Validator_internals.Trusted_validators_membership_change.Remove )
      in
      let address =
        match address with
        | None -> failwith "give me an address!!!"
        | Some address -> address in
      let trusted_change =
        Validator_internals.Trusted_validators_membership_change.Set.remove
          { address; action } x in
      { t with trusted_change = Some trusted_change }

  (*********************************************************************)
  (* LAST CHANGE TIMESTAMP                                             *)
  (*********************************************************************)

  let get_last_change_timestamp : t -> float =
   fun t -> t.last_seen_membership_change_timestamp

  let update_last_change_timestamp : float -> t -> t =
   fun timestamp t ->
    { t with last_seen_membership_change_timestamp = timestamp }

  (*********************************************************************)
  (* BLOCK PROPOSER                                                    *)
  (*********************************************************************)

  let get_new_block_proposer :
      int -> t -> Validator_internals.Validators.validator option =
   fun skips t ->
    Validator_internals.Validators.after_current skips t.validators

  let update_block_proposer : Crypto.Key_hash.t -> t -> t =
   fun block_author t ->
    (* TODO: check is sender ok *)
    {
      t with
      validators =
        Validator_internals.Validators.update_current block_author t.validators;
    }

  (*********************************************************************)
  (* VALIDATORS (list, number, etc)                                    *)
  (*********************************************************************)
  let get_number_of_validators : t -> int =
   fun t -> Validator_internals.Validators.length t.validators

  let get_validators_hash : t -> Crypto.BLAKE2B.t =
   fun t -> Validator_internals.Validators.hash t.validators

  let get_validators_list : t -> Validator_internals.Validators.validator list =
   fun t -> Validator_internals.Validators.to_list t.validators

  let get_validators_address_list : t -> Crypto.Key_hash.t list =
   fun t ->
    let validators_list = get_validators_list t in
    List.map (fun v -> v.Validator_internals.Validators.address) validators_list

  (*********************************************************************)
  (* VALIDATORS (modifying)                                            *)
  (*********************************************************************)

  let process_add_validator : string -> t -> t * Message.t list =
   fun new_validator_str t ->
    let new_validator = string_to_key new_validator_str in
    (* TODO: add msg *)
    ( {
        t with
        validators =
          Validator_internals.Validators.add
            { address = new_validator }
            t.validators;
      },
      [] )

  let process_remove_validator : string -> t -> t * Message.t list =
   fun old_validator_str t ->
    let old_validator = string_to_key old_validator_str in
    (* TODO: send response with rm validator *)
    ( {
        t with
        validators =
          Validator_internals.Validators.remove
            { address = old_validator }
            t.validators;
      },
      [] )

  (*********************************************************************)
  (* MANDATORY                                                         *)
  (*********************************************************************)

  let tick _timestamp t = (t, [])

  let process_message : Message.t -> t -> t * Message.t list =
   fun msg t ->
    (* TODO: verify sender is allowed to offer actions. *)
    (* TODO: verify payload sig *)
    let payload_bytes : bytes = Message.get_payload msg in
    let payload : Action.t =
      Pollinate.Util.Encoding.unpack Action.bin_read_t payload_bytes in
    match payload with
    | Add_validator new_validator_str ->
      process_add_validator new_validator_str t
    | Remove_validator old_validator_str ->
      process_remove_validator old_validator_str t

  let filter_msgs : Message.t list -> Message.t list =
   fun msgs ->
    let filtered =
      List.filter
        (fun msg ->
          match Message.get_sub_category_opt msg with
          | None -> false
          | Some deku_op ->
          match deku_op.operation_family with
          | Deku_operation.Validators -> true
          | _ -> false)
        msgs in
    filtered
end

module Make (P : VALIDATORS_PARAMETER) : MAIN = Raw (P)
