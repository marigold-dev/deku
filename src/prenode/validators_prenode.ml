open Watcher

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
    [@@deriving ord, bin_io]
  end

  type validator = { address : Crypto.Key_hash.t } [@@deriving eq, ord, yojson]

  let empty = Validators.empty
  type t = Validators.t [@@deriving yojson]

  let tick _timestamp t = (t, [])

  let string_to_key : string -> Crypto.Key_hash.t =
   fun my_str ->
    let key_opt = Crypto.Key_hash.of_string my_str in
    match key_opt with
    | None -> failwith "Couldn't retrive key from string!"
    | Some key -> key

  let update_block_proposer : Crypto.Key_hash.t -> t -> t =
   fun block_author t ->
    (* TODO: check is sender ok *)
    Validators.update_current block_author t

  let process_add_validator : string -> t -> t * Message.t list =
   fun new_validator_str t ->
    let new_validator = string_to_key new_validator_str in
    (* TODO: add msg *)
    (Validators.add { address = new_validator } t, [])

  let process_remove_validator : string -> t -> t * Message.t list =
   fun old_validator_str t ->
    let old_validator = string_to_key old_validator_str in
    (* TODO: send response with rm validator *)
    (Validators.remove { address = old_validator } t, [])

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
end

module Make (P : VALIDATORS_PARAMETER) : MAIN = Raw (P)
