module type PROXY_POLLINATE = sig
  type t
  val of_deku : Message.t -> Pollinate.Node.Message.t
  val of_pollinate : Pollinate.Node.Message.t -> Message.t

  val handle_outgoing_messages :
    Pollinate.Node.Inbox.t -> Message.t list -> Pollinate.Node.Inbox.t
  val handle_ingoing_messages :
    Pollinate.Node.Inbox.t -> Message.t * Pollinate.Node.Inbox.t
end
