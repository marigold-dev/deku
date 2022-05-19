module type PROXY_POLLINATE = sig
  type t

  val of_deku : Message.t -> Pollinate.Node.Message.t

  val of_pollinate : Pollinate.Node.Message.t -> Message.t

  val preprocess : Pollinate.Node.Message.t -> Pollinate.Node.Message.t
  (** Properly encodes requests and responses to ensure category definition and such. *)

  val msg_handler : Pollinate.Node.Message.t -> bytes
  (** Business logic: Pollinate client transforms Pollinate messages into the appropriate bytes output to be sent on the network. *)

  val received_messages_of_pollinate : t -> Message.t list * t
  (** Consumes a message received via Pollinate and transforms it into relevant Deku Message list. *)

  val received_messages_of_deku : Message.t list -> Message.t list * t
  (** Consumes Deku Message list and alters state so that appropriate response/request are sent to the network through Pollinate. *)
end

module PROXY_POLLINATE = struct
  (* let of_deku : Message.t -> Pollinate.Node.Message.t = fun msg ->
     {
       category = Pollinate.Node.Message.Request ;
       sub_category_opt = Message.get_sub_category_opt msg ;
       id = 1 ;
       payload = Message.get_payload msg ;
       sender = Message.get_sender msg ;
       recipient = Message.get_recipient_opt msg ;
     } *)

  let preprocess : Pollinate.Node.Message.t -> Pollinate.Node.Message.t =
   fun msg -> msg

  let incoming_consensus_governance_operation :
      Pollinate.Node.Message.t -> Message.t =
   fun msg ->
    (* Logique *)
    match msg.Pollinate.Node.Message.category with
    | Request -> failwith "Consensus governance request op"
    | Response -> failwith "Consensus governance responce op"
    | _ -> failwith "Consensus governance unknown op"

  let msg_handler : Pollinate.Node.Message.t -> bytes =
   fun _msg ->
    (* match msg.Pollinate.Node.Message.sub_category_opt with
       | Some ("Consensus_governance_operation", _) -> incoming_consensus_governance_operation msg
       | _ -> *)
    failwith "Bored now"

  let deku_pollinate_node =
    Lwt_main.run
      (Pollinate.Node.init ~preprocess:preprocess ~msg_handler ("", 3000))
end
