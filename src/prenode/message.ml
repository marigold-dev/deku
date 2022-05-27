type t = {
  operation : Deku_operation.t;
      (** (Consensus, Proposal) or (Consensus_governance, Add_validator) *)
  payload : bytes;
  recipient : Deku_operation.operation_family;
}
[@@deriving eq, ord]
(** Those messages only exist inside Deku. *)

let get_operation t = t.operation
let get_payload t = t.payload

let get_recipient t = t.recipient

let make :
    operation:Deku_operation.t ->
    payload:bytes ->
    recipient:Deku_operation.operation_family ->
    t =
 fun ~operation ~payload ~recipient -> { operation; payload; recipient }
