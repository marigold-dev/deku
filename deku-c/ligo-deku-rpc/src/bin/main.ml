open Eio.Std
open Piaf
open Ligo_deku_rpc

let request_handler ~env =
  let router = Router.router ~env () in
  fun req ->
    let { Server.ctx = _; request } = req in
    Logs.debug (fun m -> m "Request to %s" request.target);
    Routes.match' router ~target:request.target |> function
    | Routes.NoMatch -> Piaf.Response.create `Not_found
    | FullMatch handler | MatchWithTrailingSlash handler -> handler req

let main port =
  let config = Server.Config.create port in
  Eio_main.run (fun env ->
      Switch.run (fun sw ->
          let server =
            Server.create ~config
              (Middlewares.cors_middleware @@ request_handler ~env)
          in
          let _command =
            Server.Command.start ~bind_to_address:Eio.Net.Ipaddr.V4.any ~sw env
              server
          in
          ()))

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  setup_log Logs.Info;
  let port = ref 9090 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number (9090 by default)") ]
    ignore "Echoes POST requests. Runs forever.";
  main !port