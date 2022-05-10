module type PROXY_POLLINATE = sig
  type t
  val of_deku : Prenodemessage.t -> Pollinate.Node.Message.t
  val of_pollinate : Pollinate.Node.Message.t -> Prenodemessage.t

  val handle_outgoing_messages :
    Pollinate.Node.Inbox.t -> Prenodemessage.t list -> Pollinate.Node.Inbox.t
  val handle_ingoing_messages :
    Pollinate.Node.Inbox.t -> Prenodemessage.t * Pollinate.Node.Inbox.t
end
