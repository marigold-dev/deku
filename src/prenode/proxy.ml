module type PROXY_POLLINATE = sig
  type t

  val of_deku : Message.t -> Pollinate.PNode.Message.t

  val of_pollinate : Pollinate.PNode.Message.t -> Message.t

  val preprocess : Pollinate.PNode.Message.t -> Pollinate.PNode.Message.t
  (** Properly encodes requests and responses to ensure category definition and such. *)

  val received_messages_of_pollinate : t -> Message.t list * t
  (** Consumes a message received via Pollinate and transforms it into relevant Deku Message list. *)

  val received_messages_of_deku : Message.t list -> Message.t list * t
  (** Consumes Deku Message list and alters state so that appropriate response/request are sent to the network through Pollinate. *)
end

module PROXY_POLLINATE = struct
  let preprocess : Pollinate.PNode.Message.t -> Pollinate.PNode.Message.t =
   fun msg -> msg

  let incoming_consensus_governance_operation :
      Pollinate.PNode.Message.t -> Message.t =
   fun msg ->
    (* Logique *)
    match msg.Pollinate.PNode.Message.category with
    | Request -> failwith "Consensus governance request op"
    | _ -> failwith "Consensus governance unknown op"

  (* TODO: Ack is not ok, we should send back a way to ID what msg we ack for. *)
  type response = Ack [@@deriving bin_io]

  let msg_handler : Pollinate.PNode.Message.t -> bytes =
   fun msg ->
    (* TODO: verify signature, reject wrong signatures. *)
    match msg.Pollinate.PNode.Message.sub_category_opt with
    | None -> failwith "Sub category is mandatory in deku"
    | Some (family, _) ->
    match family with
    | "Consensus_govenance" ->
      let _ = incoming_consensus_governance_operation msg in
      Pollinate.Util.Encoding.pack bin_writer_response Ack
    | _ -> failwith "Bored now"

  (* TODO: implement SIGN *)
  let sign : bytes -> bytes option -> bytes option = fun _payload _key -> None

  let deku_pollinate_node =
    Lwt_main.run
      (Pollinate.PNode.init ~preprocess ~msg_handler ~sign_payload:sign
         ~key:None ("", 3000))
end
