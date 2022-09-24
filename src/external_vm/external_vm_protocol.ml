open Deku_tezos
open Deku_crypto
module String_map = Map.Make (String)

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
             let v_json = Yojson.Safe.from_string v in
             (k, v_json))
    in
    `Assoc assoc

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `Assoc l ->
        List.to_seq l
        |> Seq.map (fun (k, v) -> (k, Yojson.Safe.to_string v))
        |> String_map.of_seq
    | _ -> failwith "FIXME: what to do here?"
end

type set = { key : string; value : string } [@@deriving yojson]

type transaction = {
  source : Key_hash.t;
  operation : string;
  tickets : (Ticket_id.t * int64) list;
}
[@@deriving yojson]

type vm_client_message =
  | Transaction of transaction
  | Set of set
  | Get_Initial_State
  | Set_Initial_State of State.t
  | Get of string
[@@deriving yojson]

type vm_server_message =
  | Init of set list
  | Stop
  | Set of set
  | Error of string
[@@deriving yojson]
