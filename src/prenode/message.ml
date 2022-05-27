type t = {
  operation : Deku_operation.t;
      (** (Consensus, Proposal) or (Consensus_governance, Add_validator) *)
  payload : bytes;
  recipient : Deku_operation.operation_family;
  signature_opt : bytes option;
}
[@@deriving eq, ord]
(** Those messages only exist inside Deku. *)

let get_operation t = t.operation
let get_payload t = t.payload

let get_recipient t = t.recipient

let pack : 'a Bin_prot.Type_class.writer -> 'a -> bytes =
 fun writer payload ->
  let open Bin_prot.Utils in
  let open Bin_prot.Common in
  let buf = bin_dump ~header:true writer payload in
  let len = buf_len buf in
  let byte_buff = Bytes.create len in
  blit_buf_bytes buf byte_buff ~len;
  byte_buff

let size_header_length = 8

let read_all reader buf = reader buf ~pos_ref:(ref 0)

let prepare_buff byte_buff =
  let open Bin_prot.Common in
  let len = Bytes.length byte_buff - size_header_length in
  let buf = create_buf len in
  blit_bytes_buf ~src_pos:size_header_length byte_buff buf ~len;
  buf

let unpack : 'a Bin_prot.Read.reader -> bytes -> 'a =
 fun reader payload ->
  let buf = prepare_buff payload in
  read_all reader buf

let make :
    operation:Deku_operation.t ->
    payload:bytes ->
    recipient:Deku_operation.operation_family ->
    signature_opt:bytes option ->
    t =
 fun ~operation ~payload ~recipient ~signature_opt ->
  { operation; payload; recipient; signature_opt }
