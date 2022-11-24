open Handlers

type handler = state:Api_state.t -> body:string -> Piaf.Response.t
type server = { get : handler Routes.router; post : handler Routes.router }
type t = server

let empty =
  let empty = Routes.one_of [] in
  { get = empty; post = empty }

let json_to_response ~status body =
  let body = body |> Data_encoding.Json.to_string |> Piaf.Body.of_string in
  let headers =
    Piaf.Headers.of_list
      [ (Piaf.Headers.Well_known.content_type, "application/json") ]
  in
  Piaf.Response.create ~headers ~body status

let error_to_response error =
  let status = Api_error.to_http_code error |> Piaf.Status.of_code in
  let body = Api_error.encoding error in
  json_to_response ~status body

let handle_error error route =
  let code = Api_error.to_http_code error in
  let str = Api_error.string_of_error error in
  if code >= 500 then
    Logs.err (fun m -> m "%s: %s" str (Routes.string_of_route route))
  else Logs.warn (fun m -> m "%s: %s" str (Routes.string_of_route route));
  error_to_response error

let add_route route meth server =
  match meth with
  | `GET -> { server with get = Routes.add_route route server.get }
  | `POST -> { server with post = Routes.add_route route server.post }

let with_body (module Handler : HANDLERS) server =
  let route =
    Routes.map
      (fun path ~state ~body ->
        let body = Data_encoding.Json.from_string body in
        match body with
        | Error err -> error_to_response (Api_error.invalid_body err)
        | Ok body -> (
            let body = Data_encoding.Json.destruct Handler.body_encoding body in
            let response = Handler.handler ~path ~body ~state in
            match response with
            | Error error -> handle_error error Handler.route
            | Ok response ->
                let body =
                  Data_encoding.Json.construct Handler.response_encoding
                    response
                in
                json_to_response ~status:`OK body))
      Handler.route
  in
  add_route route Handler.meth server

let without_body (module Handler : NO_BODY_HANDLERS) server =
  let route =
    Routes.map
      (fun path ~state ~body:_ ->
        let response = Handler.handler ~path ~state in
        match response with
        | Error error -> handle_error error Handler.route
        | Ok response ->
            let body =
              Data_encoding.Json.construct Handler.response_encoding response
            in
            json_to_response ~status:`OK body)
      Handler.route
  in
  add_route route Handler.meth server

let make_handler ~state server request =
  let Piaf.Server.Handler.{ request : Piaf.Request.t; _ } = request in
  let target = request.target in
  let meth = request.meth in
  let body = request.body |> Piaf.Body.to_string in
  match body with
  | Error err ->
      let error = Api_error.invalid_body (Piaf.Error.to_string err) in
      let str = Api_error.string_of_error error in
      Logs.warn (fun m -> m "%s: %s" str error.msg);
      error_to_response error
  | Ok body -> (
      let matched_route =
        match meth with
        | `GET -> Ok (Routes.match' ~target server.get)
        | `POST -> Ok (Routes.match' ~target server.post)
        | _ -> Error (Api_error.method_not_allowed target meth)
      in
      match matched_route with
      | Ok (Routes.FullMatch handler)
      | Ok (Routes.MatchWithTrailingSlash handler) ->
          handler ~state ~body
      | Ok Routes.NoMatch ->
          let error = Api_error.endpoint_not_found target in
          let str = Api_error.string_of_error error in
          Logs.warn (fun m -> m "%s: %s" str request.target);
          error_to_response error
      | Error error ->
          Logs.warn (fun m -> m "Matching error: %s" error.msg);
          error_to_response error)
