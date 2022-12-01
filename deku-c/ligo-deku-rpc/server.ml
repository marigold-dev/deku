(* Copied from the deku-api server *)

open Handlers

type handler =
  env:Eio.Stdenv.t ->
  params:(string * string list) list ->
  body:string ->
  Piaf.Response.t

type server = { get : handler Routes.router; post : handler Routes.router }
type t = server

let empty =
  let empty = Routes.one_of [] in
  { get = empty; post = empty }

let json_to_response ~status (body : Data_encoding.json) =
  let body = Data_encoding.Json.to_string body |> Piaf.Body.of_string in
  let headers =
    Piaf.Headers.of_list
      [ (Piaf.Headers.Well_known.content_type, "application/json") ]
  in
  Piaf.Response.create ~headers ~body status

let error_to_response error =
  let status = Piaf.Status.of_code 500 in
  let body = `String error in
  json_to_response ~status body

let add_route route meth server =
  match meth with
  | `GET -> { server with get = Routes.add_route route server.get }
  | `POST -> { server with post = Routes.add_route route server.post }

let with_body (module Handler : HANDLERS) server =
  let route =
    Routes.map
      (fun path ~env ~params ~body ->
        let result =
          body |> Data_encoding.Json.from_string
          |> Result.map_error (fun err -> error_to_response err)
          |> fun res ->
          Result.bind res (fun body ->
              try Ok (Json_encoding.destruct Handler.body_encoding body)
              with exn ->
                let msg =
                  match exn with
                  | Data_encoding.Json.Bad_array_size _ -> "Wrong array size"
                  | Data_encoding.Json.Cannot_destruct (paths, _) ->
                      List.fold_left
                        (fun acc path ->
                          let err =
                            match path with
                            | `Field path ->
                                Format.sprintf "cannot parse field: [%s]" path
                            | `Index index ->
                                Format.sprintf "wrong index of array: [%i]"
                                  index
                            | `Star -> "every fields"
                            | `Next -> "next"
                          in
                          err ^ "," ^ acc)
                        "" paths
                  | Data_encoding.Json.No_case_matched _ -> "no case matched"
                  | Data_encoding.Json.Unexpected (field, _) ->
                      Format.sprintf "unexpected: [%s]" field
                  | Data_encoding.Json.Unexpected_field field ->
                      Format.sprintf "unexpected field: [%s]" field
                  | exn -> Printexc.to_string exn
                in
                Error (error_to_response msg))
        in
        match result with
        | Error err -> err
        | Ok body -> (
            let response = Handler.handler ~env ~path ~params ~body in
            match response with
            | Error error -> error_to_response error
            | Ok response ->
                let body =
                  Json_encoding.construct Handler.response_encoding response
                in
                json_to_response ~status:`OK body))
      Handler.route
  in
  add_route route Handler.meth server

let without_body (module Handler : NO_BODY_HANDLERS) server =
  let route =
    Routes.map
      (fun path ~env ~params ~body:_ ->
        let response = Handler.handler ~env ~path ~params in
        match response with
        | Error error -> error_to_response error
        | Ok response ->
            let body =
              Data_encoding.Json.construct Handler.response_encoding response
            in
            json_to_response ~status:`OK body)
      Handler.route
  in
  add_route route Handler.meth server

let make_handler ~env server request =
  let Piaf.Server.Handler.{ request : Piaf.Request.t; _ } = request in
  let target = request.target in
  let meth = request.meth in
  let body = request.body |> Piaf.Body.to_string in
  let uri = Piaf.Request.uri request in
  let params = Uri.query uri in
  match body with
  | Error err -> Piaf.Error.to_string err |> error_to_response
  | Ok body -> (
      let matched_route =
        match meth with
        | `GET -> Ok (Routes.match' ~target server.get)
        | `POST -> Ok (Routes.match' ~target server.post)
        | _ -> Error "Method not allowed"
      in
      match matched_route with
      | Ok (Routes.FullMatch handler)
      | Ok (Routes.MatchWithTrailingSlash handler) ->
          handler ~env ~params ~body
      | Ok Routes.NoMatch -> error_to_response "Not a valid a route"
      | Error error -> error |> error_to_response)
