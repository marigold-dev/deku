open Deku_crypto
module String_map = Map.Make (String)
open Deku_stdlib

type set = { key : string; value : string } [@@deriving yojson]

module State = struct
  type t = string String_map.t

  let get = String_map.find_opt
  let set = String_map.add
  let empty = String_map.empty
  (* let equal = Storage.equal Yojson.Safe.equal *)

  let yojson_of_t map =
    let assoc =
      String_map.bindings map
      |> List.map (fun (k, v) ->
             (* FIXME: doing this for convenience for now, but it seems
                like a bad idea in the long run. We should make the protocol
                agnostic of the serialization format. *)
             let v_json = `String v in
             (k, v_json))
    in
    `Assoc assoc

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `Assoc l ->
        List.to_seq l
        |> Seq.map (fun (k, v) ->
               match v with
               | `String v -> (k, v)
               | _ -> failwith "FIXME: better error message")
        |> String_map.of_seq
    | _ -> failwith "FIXME: what to do here?"
end

module Make (Ticket_id : sig
  type t [@@deriving yojson]
end) (Address : sig
  type t [@@deriving yojson]
end) =
struct
  type transaction = {
    operation_raw_hash : string;
    source : Key_hash.t;
    operation : string;
    tickets : (Ticket_id.t * N.t) list;
    level : Deku_concepts.Level.t;
  }
  [@@deriving yojson]

  type vm_client_message =
    | Transaction of transaction
    | Noop_transaction
    | Set of set
    | Get_Initial_State
    | Give_Tickets of (Ticket_id.t * N.t) list
    | Set_Initial_State of State.t
    | Get of string
  [@@deriving yojson]

  type vm_server_message =
    | Init of set list
    | Stop
    | Set of set
    | Take_tickets of Address.t
    | Deposit_tickets of {
        address : Address.t;
        tickets : (Ticket_id.t * N.t) list;
      }
    | Error of string
  [@@deriving yojson]
end
