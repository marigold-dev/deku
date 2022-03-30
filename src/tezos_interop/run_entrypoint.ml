open Helpers
open Crypto
open Tezos

type id = int [@@deriving yojson]
let initial_id = 0
module Id_map = Map.Make (Int)

module Request = struct
  type transaction = {
    rpc_node : string;
    secret : string;
    confirmation : int;
    destination : string;
    entrypoint : string;
    payload : Yojson.Safe.t;
  }

  type t = {
    id : id;
    content : transaction;
  }

  let to_yojson request =
    let module T = struct
      type t = {
        id : id;
        rpc_node : string;
        secret : string;
        confirmation : int;
        destination : string;
        entrypoint : string;
        payload : Yojson.Safe.t;
      }
      [@@deriving to_yojson]
    end in
    let { id; content } = request in
    let { rpc_node; secret; confirmation; destination; entrypoint; payload } =
      content in
    T.to_yojson
      { id; rpc_node; secret; confirmation; destination; entrypoint; payload }
end

module Response = struct
  type transaction =
    | Applied     of { hash : string }
    | Failed      of { hash : string }
    | Skipped     of { hash : string }
    | Backtracked of { hash : string }
    | Unknown     of { hash : string }
    | Error       of string
  type t = {
    id : id;
    content : transaction;
  }

  let transaction_of_yojson json =
    let module T = struct
      type t = { status : string }
      and finished = { hash : string }
      and error = { error : string } [@@deriving of_yojson { strict = false }]
    end in
    let finished make =
      let%ok { hash } = T.finished_of_yojson json in
      Ok (make hash) in
    let%ok { status } = T.of_yojson json in
    match status with
    | "applied" -> finished (fun hash -> Applied { hash })
    | "failed" -> finished (fun hash -> Failed { hash })
    | "skipped" -> finished (fun hash -> Skipped { hash })
    | "backtracked" -> finished (fun hash -> Backtracked { hash })
    | "unknown" -> finished (fun hash -> Unknown { hash })
    | "error" ->
      let%ok { error } = T.error_of_yojson json in
      Ok (Error error)
    | _ -> Error "invalid status"

  let of_yojson json =
    let module T = struct
      type header = { id : int } [@@deriving of_yojson { strict = false }]
    end in
    let%ok { id } = T.header_of_yojson json in
    let%ok content = transaction_of_yojson json in
    Ok { id; content }
end

module Process : sig
  type t
  val spawn : unit -> t
  val request : t -> Request.transaction -> Response.transaction Lwt.t
end = struct
  exception Failed_to_parse_json of string
  exception Process_closed of Unix.process_status
  exception Unknown_response_id of Response.t

  type t = {
    mutable next_id : id;
    mutable request_promise : Request.t Lwt.t;
    mutable request_resolver : Request.t Lwt.u;
    mutable response_pending : Response.transaction Lwt.u Id_map.t;
  }

  let make () =
    let request_promise, request_resolver = Lwt.wait () in
    {
      next_id = initial_id;
      request_promise;
      request_resolver;
      response_pending = Id_map.empty;
    }

  let on_fail exn =
    (* TODO: https://github.com/marigold-dev/deku/issues/502 *)
    Format.eprintf "tezos_interop failure: %s\n%!" (Printexc.to_string exn);
    exit 1

  let spawn_node_process ~file ~to_yojson ~of_yojson input_stream =
    let node = "node" in
    let command = (node, [|node; file|]) in

    let process = Lwt_process.open_process command in

    (* handle process failure *)
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            (* this should never return *)
            let%await status = process#status in
            raise (Process_closed status))
          on_fail);

    (* TODO: test that exception on input and output kill the node *)
    let input_stream =
      Lwt_stream.map
        (fun input ->
          let json = to_yojson input in
          Yojson.Safe.to_string json)
        input_stream in
    (* handle input exceptions *)
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> Lwt_io.write_lines process#stdin input_stream)
          on_fail);

    let output_stream = Lwt_io.read_lines process#stdout in
    let output_stream =
      Lwt_stream.map
        (fun line ->
          Format.eprintf "Run_entrypoint: %s\n%!" line;
          (* TODO: are exceptions on Lwt_stream.map safe? *)
          let json = Yojson.Safe.from_string line in
          match of_yojson json with
          | Ok value -> value
          | Error error -> raise (Failed_to_parse_json error))
        output_stream in
    (* handle output exceptions *)
    let output_stream =
      Lwt_stream.map
        (function
          | Ok value -> value
          | Error exn -> on_fail exn)
        (Lwt_stream.wrap_exn output_stream) in
    output_stream

  let handle_output process response =
    let id = response.Response.id in
    match Id_map.find_opt id process.response_pending with
    | Some resolver -> Lwt.wakeup_later resolver response.content
    | None -> raise (Unknown_response_id response)
  let handle_outputs process output_stream =
    Lwt_stream.iter (fun output -> handle_output process output) output_stream

  let spawn () =
    let t = make () in

    let file = Scripts.file_run_entrypoint in
    let input_stream =
      Lwt_stream.from (fun () ->
          let%await request = t.request_promise in
          Lwt.return (Some request)) in
    let output_stream =
      spawn_node_process ~file ~to_yojson:Request.to_yojson
        ~of_yojson:Response.of_yojson input_stream in

    Lwt.async (fun () ->
        Lwt.catch (fun () -> handle_outputs t output_stream) on_fail);

    t

  let request t content =
    let id = t.next_id in
    let request_resolver = t.request_resolver in
    let new_request_promise, new_request_resolver = Lwt.wait () in
    let response_promise, response_resolver = Lwt.wait () in
    let response_pending = Id_map.add id response_resolver t.response_pending in

    t.next_id <- id + 1;
    t.request_promise <- new_request_promise;
    t.request_resolver <- new_request_resolver;
    t.response_pending <- response_pending;

    let request = Request.{ id; content } in
    (* resolved only after updating t.request_resolver *)
    Lwt.wakeup_later request_resolver request;

    response_promise
end

let run t ~rpc_node ~secret ~required_confirmations ~destination ~entrypoint
    ~payload =
  Process.request t
    {
      rpc_node = Uri.to_string rpc_node;
      secret = Secret.to_string secret;
      confirmation = required_confirmations;
      destination = Address.to_string destination;
      entrypoint;
      payload;
    }
