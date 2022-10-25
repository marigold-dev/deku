(* Add the middleware *)
let cors_middleware handler req =
  let response : Piaf.Response.t = handler req in
  let add_header name value headers = Piaf.Headers.add headers name value in
  let headers =
    response.headers
    |> add_header "Access-Control-Allow-Origin" "*"
    |> add_header "Access-Control-Allow-Headers" "*"
    |> add_header "Allow" "*"
  in
  let response =
    Piaf.Response.create ~version:response.version ~headers ~body:response.body
      response.status
  in
  response

(* Middleware to disable the cache *)
let no_cache_middleware handler req =
  let response : Piaf.Response.t = handler req in
  let headers =
    Piaf.Headers.add response.headers "Cache-Control"
      "max-age=0, no-cache, no-store"
  in
  let response =
    Piaf.Response.create ~version:response.version ~headers ~body:response.body
      response.status
  in
  response
