open Protocol
open Helpers

module type PARAMETERS = sig
  val node_uri : Uri.t

  val port : int
end

module type Request_endpoint = sig
  type request [@@deriving yojson]

  type response [@@deriving yojson]

  val path : string

  val handle : request -> (response, string) result Lwt.t
end

let handle_request (type req res)
    (module E : Request_endpoint with type request = req and type response = res)
    =
  let handler request =
    let%await body = Dream.body request in
    let request = E.request_of_yojson (Yojson.Safe.from_string body) in
    match request with
    | Ok request -> (
      let%await response = E.handle request in
      match response with
      | Ok response ->
        E.response_to_yojson response |> Yojson.Safe.to_string |> Dream.json
      | Error _ -> failwith "todo1")
    | Error err -> failwith err in
  Dream.post E.path handler

(** retrieve the level of the chain **)
module Level = struct
  type request = unit [@@deriving yojson]

  type response = { level : int64 } [@@deriving yojson]

  let path = "/block-level"

  let handle () =
    let level = Repository.level () in
    await (Ok { level })
end

(* get a specific block *)
module Get_block_by_level = struct
  type request = { level : int64 } [@@deriving yojson]

  type response = Block.t [@@deriving yojson]

  let path = "/block-by-level"

  let handle { level } =
    let%await block = Repository.find_block_by_level level in
    match block with
    | Some block -> await (Ok block)
    | None -> await (Error "block not found")
end

(* to check if the indexer is boostrapped *)
module Bootstrapped (Parameters : PARAMETERS) = struct
  type request = unit [@@deriving yojson]

  type response = { is_sync : bool } [@@deriving yojson]

  let path = "/is-sync"

  let handle () =
    let%await is_sync = Interval.is_sync Parameters.node_uri in
    await (Ok { is_sync })
end

let run (module Parameters : PARAMETERS) =
  Dream.serve ~port:Parameters.port
  @@ Dream.logger
  @@ Dream.router
       [
         handle_request (module Level);
         handle_request (module Get_block_by_level);
         handle_request (module Bootstrapped (Parameters));
       ]
