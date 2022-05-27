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

  (* TODO: Ack is not ok, we should send back a way to ID what msg we ack for. *)
  type response = Ack [@@deriving bin_io]

  (* (* (* POST /trusted-validators-membership *)
     (* Add or Remove a new trusted validator *)
     let handle_trusted_validators_membership =
       handle_request
         (module Network.Trusted_validators_membership_change)
         (fun update_state request ->
           Flows.trusted_validators_membership (Server.get_state ()) update_state
             request) *)

             let process_validators_msg : Pollinate.PNode.Message.t -> bytes =
              fun msg ->
               (* Logique *)
               let operation_family_str, operation_detail_str =
                 match msg.Pollinate.PNode.Message.sub_category_opt with
                 | None -> failwith "Sub category is mandatory in deku"
                 | Some (family, detail) -> (family, detail) in
               let operation_family = Deku_operation.family_of_string operation_family_str in
               let operation_detail = Deku_operation.detail_of_string operation_detail_str in
               let operation = Deku_operation.make ~operation_family ~operation_detail in
               let msg =
                 Message.make ~operation ~payload:msg.Pollinate.PNode.Message.payload
                   ~recipient:operation_family in

               let node_state = Node.Server.get_state () in
               Flows.update_validators (Server.get_state ()) update_state in
  *)

  let msg_handler : Pollinate.PNode.Message.t -> bytes =
   fun msg ->
    (* TODO: verify signature, reject wrong signatures. *)
    match
      ( msg.Pollinate.PNode.Message.category,
        msg.Pollinate.PNode.Message.sub_category_opt )
    with
    | Request, Some (family, _) -> (
      match family with
      | "Validators" -> failwith "TODO"
      (* let response = process_validators_msg msg in
         response *)
      | _ -> failwith "Bored now")
    | _, Some (_, _) -> failwith "I am expecting a request"
    | _, None -> failwith "Sub category is mandatory in deku"

  (* TODO: implement SIGN *)
  let sign : bytes -> bytes option -> bytes option = fun _payload _key -> None

  let deku_pollinate_node =
    Lwt_main.run
      (Pollinate.PNode.init ~preprocess ~msg_handler ~sign_payload:sign
         ~key:None ("", 3000))
end
