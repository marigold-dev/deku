module Contract_operation : sig
  type t =
    | Transfer of {
        destination : Address.t;
        ticket : Ticket_id.t;
        amount : Amount.t;
      }
    | Invoke   of {
        param : bytes;
        destination : Address.t;
        tickets : (Ticket_id.t * Amount.t) list;
      }
end

module type CTX = sig
  module Address : sig
    type t = Address.t

    val size : int

    val of_bytes : bytes -> t

    val to_bytes : t -> bytes
  end

  module Ticket_handle : sig
    type t = Ticket_handle.t

    val size : int

    val of_bytes : bytes -> t

    val to_bytes : t -> bytes
  end

  module Ticket_id : sig
    type t = Ticket_id.t

    val size : t -> int

    val to_bytes : t -> bytes
  end

  module Amount : sig
    type t = Amount.t

    val size : int

    val of_int : int -> t

    val to_int : t -> int
  end

  val get_tickets : unit -> (Ticket_id.t * Amount.t) Seq.t

  val get_ops :
    int list -> (Contract_operation.t list, [`Execution_error]) result

  val get_source : Address.t

  val sender : unit -> Address.t

  val source : unit -> Address.t

  val self : unit -> Address.t

  val read_ticket : Ticket_handle.t -> Ticket_id.t * Amount.t * Ticket_handle.t

  val split_ticket :
    Ticket_handle.t * Amount.t * Amount.t -> Ticket_handle.t * Ticket_handle.t

  val join_tickets : Ticket_handle.t * Ticket_handle.t -> Ticket_handle.t

  val own_ticket : Ticket_handle.t -> Ticket_handle.t

  val get_contract_opt : Address.t -> Address.t option

  val transaction : bytes * (Ticket_handle.t * Amount.t) * Address.t -> int
end

val make :
  sender:Address.t ->
  table:(Ticket_id.t * Amount.t) Seq.t ->
  tickets:(Ticket_id.t * Amount.t) Seq.t ->
  self:Contract_address.t ->
  source:Address.t ->
  contracts_table:(Contract_address.t -> Address.t option) ->
  (module CTX)
