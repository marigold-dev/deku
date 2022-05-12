open Lwt_ext.Let_syntax

(* lazy because Unix.fork stops working after domains start *)
(* TODO: proper number of domains *)
(* THIS IS GLOBAL STATE OUTSIDE OF src/node/server.ml. CAUTION *)
let pool = lazy (Lwt_domain.setup_pool 8)
let pool () = Lazy.force_val pool

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
let decode of_yojson data =
  let%await data =
    Lwt_domain.detach (pool ()) (fun () -> decode of_yojson data) () in
  match data with
  | Ok data -> await data
  | Error exn -> raise exn
