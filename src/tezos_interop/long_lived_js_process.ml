(** Interface between a long lived JS script and an OCaml consumer *)
(* WARNING: this is higly mutable, so be careful *)

open Helpers

module Id : sig
  type t [@@deriving yojson]
  val pp : Format.formatter -> t -> unit

  val initial : t
  val next : t -> t
  module Map : Map.S with type key = t
end = struct
  type t = int [@@deriving yojson]
  let pp fmt t = Format.fprintf fmt "%d" t
  let initial = 0
  let next t = t + 1
  module Map = Map.Make (Int)
end

exception Process_closed of Unix.process_status
exception Failed_to_parse_json of string
exception Duplicated_id of Id.t
exception Unknown_id of Id.t * Yojson.Safe.t

(* enhance error messages *)
let () =
  let open Format in
  let pp_process_status fmt status =
    match status with
    | Unix.WEXITED content -> fprintf fmt "WEXITED %d" content
    | Unix.WSIGNALED content -> fprintf fmt "WSIGNALED %d" content
    | Unix.WSTOPPED content -> fprintf fmt "WSTOPPED %d" content in

  let printer = function
    | Process_closed status ->
      Some (asprintf "Process_closed (%a)" pp_process_status status)
    | Failed_to_parse_json message ->
      Some (asprintf "Failed_to_parse_json (%s)" message)
    | Duplicated_id id -> Some (asprintf "Duplicated_id (%a)" Id.pp id)
    | Unknown_id (id, json) ->
      Some
        (asprintf "Unknown_id (%a, %a)" Id.pp id
           (Yojson.Safe.pretty_print ~std:false)
           json)
    | _ -> None in
  Printexc.register_printer printer

module Message = struct
  type t = {
    id : Id.t;
    content : Yojson.Safe.t;
  }
  [@@deriving yojson]
end

module Pending : sig
  type t
  type add_error = private Duplicated_id of Id.t
  type resolve_error = private Unknown_id of Id.t

  val make : unit -> t
  val add : t -> Id.t -> Yojson.Safe.t Lwt.u -> (unit, add_error) result
  val resolve : t -> Id.t -> Yojson.Safe.t -> (unit, resolve_error) result
end = struct
  type t = Yojson.Safe.t Lwt.u Id.Map.t ref
  type add_error = Duplicated_id of Id.t
  type resolve_error = Unknown_id of Id.t

  let make () = ref Id.Map.empty
  let find t id = Id.Map.find_opt id !t
  let mem t id = Id.Map.mem id !t

  let add t id resolver =
    if mem t id then
      Error (Duplicated_id id)
    else (
      t := Id.Map.add id resolver !t;
      Ok ())

  let resolve t id json =
    match find t id with
    | Some resolver ->
      Lwt.wakeup_later resolver json;
      Ok ()
    | None -> Error (Unknown_id id)
end

type t = {
  mutable next_id : Id.t;
  (* request *)
  push : Message.t -> unit;
  (* response *)
  pending : Pending.t;
}

(* intentional, any exception in this file should kill the node *)
let raise exn =
  (* TODO: https://github.com/marigold-dev/deku/issues/502 *)
  Format.eprintf "tezos_interop failure: %s\n%!" (Printexc.to_string exn);
  exit 1

let handle_message t message =
  let Message.{ id; content } = message in
  Format.eprintf "js.message: %a\n%!"
    (Yojson.Safe.pretty_print ~std:false)
    content;
  match Pending.resolve t.pending id content with
  | Ok () -> ()
  | Error (Unknown_id id) -> raise (Unknown_id (id, content))

(* WHY: to_yojson and of_yojson here are designed so that all exceptions
   are handled here *)
let request t ~to_yojson ~of_yojson content =
  let id = t.next_id in
  let content = to_yojson content in
  let message = Message.{ id; content } in
  let promise, wakeup = Lwt.wait () in

  t.next_id <- Id.next id;
  t.push message;
  (match Pending.add t.pending id wakeup with
  | Ok () -> ()
  | Error (Duplicated_id id) -> raise (Duplicated_id id));

  let%await json = promise in
  match of_yojson json with
  | Ok value -> Lwt.return value
  | Error error -> raise (Failed_to_parse_json error)

let spawn ~file =
  let message_stream, push = Lwt_stream.create () in
  let t =
    let next_id = Id.initial in
    let push message = push (Some message) in
    let pending = Pending.make () in
    { next_id; push; pending } in

  let on_close status = raise (Process_closed status) in
  let on_error exn = raise exn in
  let input_stream =
    Lwt_stream.map (fun message -> Message.to_yojson message) message_stream
  in
  let output_stream =
    Long_lived_process.spawn ~file ~on_error ~on_close input_stream in

  (* deal with outputs an exceptions *)
  let handle_outputs () =
    Lwt_stream.iter
      (fun json ->
        let message =
          match Message.of_yojson json with
          | Ok message -> message
          | Error error -> raise (Failed_to_parse_json error) in
        handle_message t message)
      output_stream in
  Lwt.async (fun () -> Lwt.catch (fun () -> handle_outputs ()) on_error);

  t
