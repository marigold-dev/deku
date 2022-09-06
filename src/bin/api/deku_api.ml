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

type params = { database_uri : Uri.t [@env "DEKU_DATABASE_URI"] }
[@@deriving cmdliner]

let main params =
  let { database_uri } = params in
  Lwt_main.run
  @@ let%await () = Api_state.make ~database_uri in
     Dream.serve @@ Dream.router [ make_handler (module Listen_blocks) ]

let () =
  let open Cmdliner in
  let info = Cmd.info "API interface of deku." in
  let term = Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
