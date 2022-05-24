type t = {
  category : Pollinate.PNode.Message.category;
      (** Request, Response, Failure_dectection, Custom, ... *)
  sub_category_opt : Deku_operation.t option;
      (** (Consensus, Proposal) or (Consensus_governance, Add_validator) *)
  payload : bytes;
  sender : Crypto.Key_hash.t;
  recipient_opt : Crypto.Key_hash.t option;
}
[@@deriving eq, ord]
(** Those messages only exist inside Deku. *)

let get_category t = t.category

let get_sub_category_opt t = t.sub_category_opt

let get_payload t = t.payload

let get_sender t = t.sender

let get_recipient_opt t = t.recipient_opt

let make :
    category:Pollinate.PNode.Message.category ->
    sub_category_opt:Deku_operation.t option ->
    payload:bytes ->
    sender:Crypto.Key_hash.t ->
    recipient_opt:Crypto.Key_hash.t option ->
    t =
 fun ~category ~sub_category_opt ~payload ~sender ~recipient_opt ->
  { category; sub_category_opt; payload; sender; recipient_opt }
