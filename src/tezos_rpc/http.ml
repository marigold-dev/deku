open Helpers
open Error

type 'a method_ =
  | GET : unit method_
  | POST : string method_
let http_request (type a) ~uri ~(method_ : a method_) (data : a) =
  let open Piaf in
  let%await response =
    match method_ with
    | GET -> Client.Oneshot.get uri
    | POST ->
      let body = Body.of_string data in
      Client.Oneshot.post ~body uri in
  match response with
  | Ok response -> (
    let%await body = Piaf.Body.to_string response.body in
    match body with
    | Ok body -> await (Ok body)
    | Error err -> await (Error (Piaf_body err)))
  | Error err -> await (Error (Piaf_request err))

let http_request ~node_uri ~path ~method_ response_of_yojson data =
  let uri = Uri.with_path node_uri path in
  let%await body = http_request ~uri ~method_ data in
  match body with
  | Ok body -> (
    try
      let json = Yojson.Safe.from_string body in
      match response_of_yojson json with
      | Ok response -> await (Ok response)
      | Error err -> await (Error (Response_of_yojson err))
    with
    | Yojson.Json_error err -> await (Error (Json_error err)))
  | Error err -> await (Error err)

let http_get ~node_uri ~path ~of_yojson =
  http_request ~node_uri ~path ~method_:GET of_yojson ()
let http_post ~node_uri ~path ~of_yojson ~data =
  let data = Yojson.Safe.to_string data in
  http_request ~node_uri ~path ~method_:POST of_yojson data
let http_post_data_encoding ~node_uri ~path ~of_yojson ~data =
  let data = Data_encoding.Json.to_string data in
  http_request ~node_uri ~path ~method_:POST of_yojson data

(* The two functions below, make_lazy_lexbuf and lazy_json_from_stream
   exists to provide a way to continuously transform an string Lwt_stream into
   Yojson.Safe.t Lwt_stream.

     To achieve that we're using of OCaml 5.00 algebraic effects,
   as it allows to pause any function.

     In more details, every time something tries to read from this lexbuf
   if the internal buffer is empty, it will dispatch a `Poll_data` effect,
   which will have its continuation stored and resumed when `feed` is called.
   As Yojson.Safe.read_json tries to read from this lexbuf, it will stops as
   soon as there is not enough data available in the buffer and only be
   resumed when `feed` is called, `feed` is then driven by an string Lwt_stream.
*)
let make_lazy_lexbuf read =
  let open Effect in
  let module M = struct
    type _ Effect.t += Poll_data : unit Effect.t
  end in
  let open M in
  let read_buf = ref "" in
  let waiting = ref None in

  (* "" means EOL *)
  let feed s =
    read_buf := !read_buf ^ s;
    match !waiting with
    | Some cont -> Deep.continue cont ()
    | None -> () in

  let lexbuf =
    Lexing.from_function (fun buf size ->
        if String.length !read_buf = 0 then
          perform Poll_data;
        (* deref again, as perform Poll_data will mutate read_buf *)
        let s = !read_buf in
        let s_length = String.length s in
        let size = min s_length size in
        Bytes.blit_string s 0 buf 0 size;

        (* TODO: this is clearly not optimal *)
        read_buf := String.sub s size (s_length - size);
        size) in
  let handler (type a) (eff : a Effect.t) :
      ((a, unit) Deep.continuation -> unit) option =
    match eff with
    | Poll_data ->
      Some (fun (cont : (unit, unit) Deep.continuation) -> waiting := Some cont)
    | _ -> None in

  let () = Deep.try_with read lexbuf { effc = handler } in
  feed

(* TODO: what is the failure modes of this?
         Any possible memory leak?
         What happens if invalid JSON on the input?
*)
let lazy_json_from_stream string_stream =
  let pending = ref [] in

  let lexer_state = Yojson.init_lexer () in
  let rec read lexbuf =
    let open Yojson.Safe in
    let json = read_json lexer_state lexbuf in
    pending := json :: !pending;
    read lexbuf in

  let feed = make_lazy_lexbuf read in

  Lwt_stream.map_list
    (fun str ->
      feed str;

      let results = List.rev !pending in
      pending := [];
      results)
    string_stream

let http_get_listen ~uri =
  let open Piaf in
  let%await response = Client.Oneshot.get uri in
  match response with
  | Ok response ->
    (* TODO: how to cancel body_stream *)
    (* TODO: what to do with this promise?*)
    let body_stream, _promise = Piaf.Body.to_string_stream response.body in
    await (Ok body_stream)
  | Error err -> await (Error (Piaf_request err))

let http_get_listen ~node_uri ~path ~of_yojson =
  let uri = Uri.with_path node_uri path in
  let%await body_stream = http_get_listen ~uri in
  match body_stream with
  | Ok body_stream ->
    (* TODO: how to cancel and what happens if there is bad data here? *)
    let body_stream = lazy_json_from_stream body_stream in
    let body_stream =
      Lwt_stream.map
        (fun json ->
          match of_yojson json with
          | Ok output -> output
          | Error err -> raise (Request_parsing_error (Response_of_yojson err)))
        body_stream in
    await (Ok body_stream)
  | Error err -> await (Error err)
