open Deku_stdlib
open Deku_crypto

module String_map = Map.Make (struct
  type t = string

  let compare = String.compare
  let encoding = Data_encoding.string
end)

module State = struct
  type t = string String_map.t

  let get = String_map.find_opt
  let set = String_map.add
  let empty = String_map.empty
  (* let equal = Storage.equal Yojson.Safe.equal *)

  let encoding = String_map.encoding Data_encoding.string
end

open Deku_stdlib

type set = { key : string; value : string }

let set_encoding =
  let open Data_encoding in
  conv
    (fun { key; value } -> (key, value))
    (fun (key, value) -> { key; value })
    (tup2 string string)

type transaction = {
  (* raw bytes used to create the contract address *)
  operation_raw_hash : string;
  source : Key_hash.t;
  operation : string;
  tickets : (Deku_ledger.Ticket_id.t * N.t) list;
  level : Deku_concepts.Level.t;
}

let transaction_encoding =
  let open Data_encoding in
  conv
    (fun { operation_raw_hash; source; operation; tickets; level } ->
      (operation_raw_hash, source, operation, tickets, level))
    (fun (operation_raw_hash, source, operation, tickets, level) ->
      { operation_raw_hash; source; operation; tickets; level })
    (tup5 string Key_hash.encoding string
       (list (tup2 Deku_ledger.Ticket_id.encoding N.encoding))
       Deku_concepts.Level.encoding)

type vm_client_message =
  | Transaction of transaction
  | Noop_transaction
  | Set of set
  | Get_Initial_State
  | Give_Tickets of (Deku_ledger.Ticket_id.t * N.t) list
  | Set_Initial_State of State.t
  | Get of string

let vm_client_message_encoding =
  let open Data_encoding in
  union
    [
      case ~title:"Transaction" (Tag 0) transaction_encoding
        (function Transaction transaction -> Some transaction | _ -> None)
        (fun transaction -> Transaction transaction);
      case ~title:"Noop_transaction" (Tag 1) unit
        (function Noop_transaction -> Some () | _ -> None)
        (fun () -> Noop_transaction);
      case ~title:"Set" (Tag 2) set_encoding
        (function Set set -> Some set | _ -> None)
        (fun set -> Set set);
      case ~title:"Get_Initial_State" (Tag 3) unit
        (function Get_Initial_State -> Some () | _ -> None)
        (fun () -> Get_Initial_State);
      case ~title:"Give_Tickets" (Tag 4)
        (list (tup2 Deku_ledger.Ticket_id.encoding N.encoding))
        (function Give_Tickets tickets -> Some tickets | _ -> None)
        (fun tickets -> Give_Tickets tickets);
      case ~title:"Set_Initial_State" (Tag 4) State.encoding
        (function Set_Initial_State state -> Some state | _ -> None)
        (fun state -> Set_Initial_State state);
      case ~title:"Get" (Tag 6) string
        (function Get value -> Some value | _ -> None)
        (fun value -> Get value);
    ]

type vm_server_message =
  | Init of set list
  | Stop
  | Set of set
  | Take_tickets of Deku_ledger.Address.t
  | Deposit_tickets of {
      address : Deku_ledger.Address.t;
      tickets : (Deku_ledger.Ticket_id.t * N.t) list;
    }
  | Error of string

let vm_server_message_encoding =
  let open Data_encoding in
  union
    [
      case ~title:"Init" (Tag 0) (list set_encoding)
        (function Init set -> Some set | _ -> None)
        (fun set -> Init set);
      case ~title:"Stop" (Tag 1) unit
        (function Stop -> Some () | _ -> None)
        (fun () -> Stop);
      case ~title:"Set" (Tag 2) set_encoding
        (function Set set -> Some set | _ -> None)
        (fun set -> Set set);
      case ~title:"Take_tickets" (Tag 3) Deku_ledger.Address.encoding
        (function Take_tickets address -> Some address | _ -> None)
        (fun address -> Take_tickets address);
      case ~title:"Deposit_tickets" (Tag 4)
        (tup2 Deku_ledger.Address.encoding
           (list (tup2 Deku_ledger.Ticket_id.encoding N.encoding)))
        (function
          | Deposit_tickets { address; tickets } -> Some (address, tickets)
          | _ -> None)
        (fun (address, tickets) -> Deposit_tickets { address; tickets });
      case ~title:"Error" (Tag 4) string
        (function Error message -> Some message | _ -> None)
        (fun message -> Error message);
    ]
