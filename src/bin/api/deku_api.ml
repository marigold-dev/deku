open Deku_stdlib
open Handlers

let make_handler (module Handler : HANDLER) =
  let handler request =
    let%await input = Handler.input_from_request request in
    match input with
    | Error error -> Dream.json error
    | Ok input ->
        let state = Api_state.get_state () in
        let%await response = Handler.handle input state in
        let response =
          Handler.yojson_of_response response |> Yojson.Safe.to_string
        in
        Dream.json response
  in
  match Handler.meth with
  | `POST -> Dream.post Handler.path handler
  | `GET -> Dream.get Handler.path handler

let () =
  Lwt_main.run
  @@
  let database_uri = Uri.of_string "sqlite3:/tmp/database.db" in
  let%await () = Api_state.make ~database_uri in
  Dream.serve @@ Dream.router [ make_handler (module Listen_blocks) ]
