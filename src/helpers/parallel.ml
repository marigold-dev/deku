open Lwt_ext.Let_syntax
open Domainslib

(* lazy because Unix.fork stops working after domains start *)
(* TODO: proper number of domains *)
(* THIS IS GLOBAL STATE OUTSIDE OF src/node/server.ml. CAUTION *)
let domains = 32

let pool = lazy (Lwt_domain.setup_pool domains)

let domainslib_pool = lazy (Task.setup_pool ~num_additional_domains:domains ())

let pool () = Lazy.force_val pool

let domainslib_pool () = Lazy.force_val domainslib_pool

let encode to_yojson data =
  let json = to_yojson data in
  Yojson.Safe.to_string json

let encode to_yojson data =
  Lwt_domain.detach (pool ()) (fun () -> encode to_yojson data) ()

let decode of_yojson data =
  try
    let json = Yojson.Safe.from_string data in
    Ok (of_yojson json)
  with
  | exn -> Error exn

let protocol_decode_list of_yojson data =
  let length = List.length data in
  let chunk_size = max (length / domains) 1 in
  let data_by_chunks = Base.List.chunks_of data ~length:chunk_size in
  let decode_chunk chunk =
    Task.async (domainslib_pool ()) (fun () ->
        List.map (fun element -> decode of_yojson element) chunk) in

  let data_by_chunks_promise = List.map decode_chunk data_by_chunks in
  Task.run (domainslib_pool ()) (fun () ->
      data_by_chunks_promise
      |> List.concat_map (fun chunk_promise ->
             Task.await (domainslib_pool ()) chunk_promise)
      |> List.map (fun element ->
             match element with
             | Ok (Ok data) -> Ok data
             | Ok (Error error) -> Error (`Of_yojson error)
             | Error exn -> Error (`Of_exception exn)))

let decode of_yojson data =
  let%await data =
    Lwt_domain.detach (pool ()) (fun () -> decode of_yojson data) () in
  match data with
  | Ok data -> await data
  | Error exn -> raise exn
