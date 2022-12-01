open Handlers

type params = { port : int [@default 9090]  (** TCP port to bind on. *) }
[@@deriving cmdliner]

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

let main { port } =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let request_handler =
    cors_middleware
    @@ (Server.empty
       |> Server.with_body (module Compile_contract)
       |> Server.with_body (module Compile_invocation)
       |> Server.without_body (module Health)
       |> Server.make_handler ~env)
  in
  let config = Piaf.Server.Config.create port in
  let server = Piaf.Server.create ~config request_handler in
  let _ =
    Piaf.Server.Command.start ~bind_to_address:Eio.Net.Ipaddr.V4.any ~sw env
      server
  in
  ()

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let main () =
  setup_log Logs.Info;
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)

let () = main ()
