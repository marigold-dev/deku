open Helpers

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

let run =
  Dream.serve @@ Dream.logger @@ Dream.router [handle_request (module Level)]
