open Deku_crypto
open Deku_gossip
open Chain_genesis

type message_kind = Response | Request | Broadcast

type local_message =
  | Message of {
      from : Key_hash.t;
      to_ : Key_hash.t;
      raw_expected_hash : string;
      raw_content : string;
      kind : message_kind;
      id : Request_id.t;
    }

let empty_messages =
  List.fold_left
    (fun map validator -> Map.add validator [] map)
    Map.empty validators

let pp_message_kind = function
  | Response -> "Response"
  | Request -> "Request"
  | Broadcast -> "Broadcast"

