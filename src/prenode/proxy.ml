module type PROXY_POLLINATE = sig
  type t

  val of_deku : Message.t -> Pollinate.Node.Message.t
  val of_pollinate : Pollinate.Node.Message.t -> Message.t

  (** Properly encodes requests and responses to ensure category definition and such. *)
  val router : Pollinate.Node.Message.t -> Pollinate.Node.Message.t 

  (** Business logic: Pollinate client transforms Pollinate messages into the appropriate bytes output to be sent on the network. *)
  val msg_handler : ('a Pollinate.Node.t) -> Pollinate.Node.Message.t -> bytes

  (** Consumes a message received via Pollinate and transforms it into relevant Deku Message list. *)
  val received_messages_of_pollinate :
    t -> Message.t list * t

  (** Consumes Deku Message list and alters state so that appropriate response/request are sent to the network through Pollinate. *)
  val received_messages_of_deku :
    Message.t list -> Message.t list * t

end
