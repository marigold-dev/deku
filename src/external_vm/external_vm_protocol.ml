open Deku_concepts
open Deku_crypto
module String_map = Map.Make (String)

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
      |> List.map (fun (key, value) ->
             (* FIXME: doing this for convenience for now, but it seems
                like a bad idea in the long run. We should make the protocol
                agnostic of the serialization format. *)
             yojson_of_set { key; value })
    in
    `List assoc

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `List l ->
        List.fold_left
          (fun acc x ->
            let { key; value } = set_of_yojson x in
            String_map.add key value acc)
          String_map.empty l
    | _ -> failwith "FIXME: what to do here?"
end

type transaction = {
  (* raw bytes used to create the contract address *)
  operation_hash_raw : string;
  source : Key_hash.t;
  operation : string;
  tickets : Ticket.t list;
}
[@@deriving yojson]

type vm_client_message =
  | Transaction of transaction
  | Set of set
  | Get_Initial_State
  | Set_Initial_State of State.t
  | Get of string
[@@deriving yojson]

open Deku_concepts

type vm_server_message =
  | Init of set list
  | Stop
  | Set of set
  | Take_tickets of string
  | Deposit_tickets of { address : string; tickets : (Ticket.t * int64) list }
  | Error of string
[@@deriving yojson]
