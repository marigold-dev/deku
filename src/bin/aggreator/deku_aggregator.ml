open Deku_crypto
open Deku_concepts
open Deku_network

module Node_uri = struct
  type t = string * int

  let cmdliner_converter =
    let of_string string =
      match String.split_on_char ':' string with
      | [ host; port ] -> `Ok (host, int_of_string port)
      | _ -> `Error "cannot parse node uri"
    in
    let to_string fmt (host, port) = Format.fprintf fmt "%s:%i" host port in
    (of_string, to_string)
end

type params = {
  secret : Ed25519.Secret.t; [@env "DEKU_SECRET"]
      (** The base58-encoded secret used as the Deku-aggregator's identity. *)
  node_uri : Node_uri.t; [@env "DEKU_NODE_URI"]
      (** A comma-separated list of the validator URI's used to join the network. *)
  port : int; [@default 5550] [@env "DEKU_PORT"]  (** The port to listen on. *)
}
[@@deriving cmdliner]

(* Post an operation to the chain ? *)
let post_to_gossip ~env ~raw_content ~raw_header ~host ~port =
  let net = Eio.Stdenv.net env in
  let message = Network_message.message ~raw_header ~raw_content in
  Network_protocol.Client.connect ~net ~host ~port @@ fun connection ->
  Network_protocol.Connection.write connection message

(* Middlewares *)
(* FIXME: Duplicated code with the API *)
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

let with_path handler req =
  let Piaf.Server.Handler.{ request : Piaf.Request.t; _ } = req in
  let target = request.target in
  match target = "/operations" with
  (* Only /operations is available *)
  | true -> handler req
  | false ->
      print_endline @@ Format.sprintf "error route not found: %s" target;
      Piaf.Response.create `Not_found

let with_body handler req =
  let Piaf.Server.Handler.{ request : Piaf.Request.t; _ } = req in
  let body = Piaf.Body.to_string request.body in
  match body with
  | Error _ -> Piaf.Response.create `Internal_server_error
  | Ok body ->
      let request = Piaf.Server.Handler.{ request; ctx = body } in
      handler request

(* Handler *)
let aggregator_handler Piaf.Server.Handler.{ ctx = body; _ } =
  let _ = body in
  (* TODO: implement this aggregator and post to the chain *)
  Piaf.Response.create `Not_implemented

(* Start the server *)
let listen ~sw ~env ~port =
  let handler =
    with_path @@ with_body @@ cors_middleware @@ aggregator_handler
  in

  let config = Piaf.Server.Config.create port in
  let server = Piaf.Server.create ~config handler in
  let _ = Piaf.Server.Command.start ~sw env server in
  ()

let main params =
  let { secret; node_uri = node_domain, node_port; port } = params in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Parse the node uri *)
  let secret = Secret.Ed25519 secret in
  let _identity = Identity.make secret in

  let _ = node_domain in
  let _ = node_port in
  listen ~sw ~env ~port

let setup_log ?style_renderer ?prefix ?level () =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  (* disable all non-deku logs *)
  let prefix = prefix |> Option.value ~default:"deku" in
  List.iter
    (fun src ->
      let src_name = Logs.Src.name src in
      if
        (not (String.starts_with ~prefix src_name))
        && not (String.equal src_name "application")
      then Logs.Src.set_level src (Some Logs.Error))
    (Logs.Src.list ())

let () = setup_log ~prefix:"deku-aggregator" ~level:Logs.Debug ()

let () =
  Logs.info (fun m -> m "Starting deku-aggregator");
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)