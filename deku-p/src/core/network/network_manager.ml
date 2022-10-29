open Deku_crypto
open Deku_concepts
open Deku_gossip
open Network_protocol

type network = {
  identity : Identity.t;
  mutable connection_id : Connection_id.t;
  (* TODO: Hashtbl would do a great job here *)
  mutable connections : (Network_message.message -> unit) Connection_id.Map.t;
  mutable connected_to : (Network_message.message -> unit) Key_hash.Map.t;
}

type t = network

let make ~identity =
  {
    identity;
    connection_id = Connection_id.initial;
    connections = Connection_id.Map.empty;
    connected_to = Key_hash.Map.empty;
  }

let create_connection network =
  let connection_id = network.connection_id in
  network.connection_id <- Connection_id.next connection_id;
  connection_id

let set_connection ~connection_id ~owner ~write network =
  let key_hash = Key_hash.of_key owner in
  network.connections <-
    Connection_id.Map.add connection_id write network.connections;
  network.connected_to <- Key_hash.Map.add key_hash write network.connected_to

let close_connection ~connection_id network =
  network.connections <-
    Connection_id.Map.remove connection_id network.connections

let with_connection ~on_connection ~on_request ~on_message network k =
  let connection_id = create_connection network in
  let handler ~sw connection =
    let write message =
      Eio.Fiber.fork ~sw @@ fun () -> Connection.write connection message
    in
    let on_message message =
      match message with
      | Network_message.Message { raw_header; raw_content } ->
          on_message ~raw_header ~raw_content
      | Network_message.Request { raw_header; raw_content } ->
          on_request ~connection:connection_id ~raw_header ~raw_content
    in
    let rec loop () =
      let message = Connection.read connection in
      on_message message;
      loop ()
    in
    let owner = Connection.owner connection in
    set_connection ~connection_id ~owner ~write network;
    on_connection ~connection:connection_id;
    loop ()
  in
  let handler connection =
    Fun.protect
      ~finally:(fun () -> close_connection ~connection_id network)
      (fun () -> Eio.Switch.run @@ fun sw -> handler ~sw connection)
  in
  k handler

let connect ~net ~clock ~host ~port ~on_connection ~on_request ~on_message
    network =
  let rec reconnect_loop ~identity ~net ~clock ~host ~port handler =
    try Network_protocol.Client.connect ~identity ~net ~host ~port handler
    with exn ->
      (* FIXME: is this properly a debug log? Or should it be warning? *)
      Logs.debug (fun m ->
          m "reconnect(%s:%d): %s" host port (Printexc.to_string exn));
      Eio.Time.sleep clock Deku_constants.reconnect_timeout;
      reconnect_loop ~identity ~net ~clock ~host ~port handler
  in
  let identity = network.identity in
  with_connection ~on_connection ~on_request ~on_message network
  @@ fun handler -> reconnect_loop ~identity ~net ~clock ~host ~port handler

let listen ~net ~clock ~port ~on_connection ~on_request ~on_message network =
  let on_error exn =
    Logs.warn (fun m -> m "listen.connection: %s" (Printexc.to_string exn))
  in
  let rec relisten ~identity ~net ~clock ~port ~on_error handler =
    try Network_protocol.Server.listen ~identity ~net ~port ~on_error handler
    with exn ->
      Logs.warn (fun m -> m "relisten: %s" (Printexc.to_string exn));
      Eio.Time.sleep clock Deku_constants.listen_timeout;
      relisten ~identity ~net ~clock ~port ~on_error handler
  in

  let handler connection =
    with_connection ~on_connection ~on_request ~on_message network
    @@ fun handler -> handler connection
  in
  let identity = network.identity in
  relisten ~identity ~net ~clock ~port ~on_error handler

let connect ~net ~clock ~nodes ~on_connection ~on_request ~on_message network =
  Eio.Fiber.List.iter
    (fun (host, port) ->
      try
        connect ~clock ~net ~host ~port ~on_connection ~on_request ~on_message
          network
      with exn ->
        Logs.warn (fun m ->
            m "connect(%s:%d): %s" host port (Printexc.to_string exn)))
    nodes

let send ~message ~write =
  (* write always includes a fork *)
  try write message
  with exn ->
    Logs.warn (fun m -> m "write.error: %s" (Printexc.to_string exn))

let broadcast message network =
  Key_hash.Map.iter
    (fun _connection write -> send ~message ~write)
    network.connected_to

let request ~raw_header ~raw_content network =
  let request = Network_message.request ~raw_header ~raw_content in
  broadcast request network

let broadcast ~raw_header ~raw_content network =
  let message = Network_message.message ~raw_header ~raw_content in
  broadcast message network

let send_request ~connection ~raw_header ~raw_content network =
  match Connection_id.Map.find_opt connection network.connections with
  | Some write ->
      let message = Network_message.request ~raw_header ~raw_content in
      send ~message ~write
  | None ->
      (* dead connection *)
      ()

let send ~connection ~raw_header ~raw_content network =
  match Connection_id.Map.find_opt connection network.connections with
  | Some write ->
      let message = Network_message.message ~raw_header ~raw_content in
      send ~message ~write
  | None ->
      (* dead connection *)
      ()

let test () =
  let open Deku_crypto in
  let open Deku_concepts in
  Eio_main.run @@ fun env ->
  let nodes = [ ("localhost", 1234); ("localhost", 1235) ] in
  let identity () =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret
  in

  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  let start ~port : unit =
    let identity = identity () in
    let network = make ~identity in
    let on_connection ~connection:_ = Logs.debug (fun m -> m "connected") in
    let on_request ~connection ~raw_header ~raw_content =
      Logs.debug (fun m ->
          m "request(%s:%.3f): %d" raw_header (Unix.gettimeofday ())
            (String.length raw_content));
      send ~connection ~raw_header ~raw_content network
    in
    let on_message ~raw_header ~raw_content =
      Logs.debug (fun m ->
          m "message(%s:%.3f): %d" raw_header (Unix.gettimeofday ())
            (String.length raw_content))
    in
    let raw_content = String.make 2_000_000 'a' in
    (* let rec loop counter =
         Eio.Fiber.yield ();
         Eio.Fiber.both
           (fun () ->
             let raw_header = Format.sprintf "rh%d" counter in
             request ~raw_header ~raw_content network)
           (fun () ->
             let raw_header = Format.sprintf "sh%d" counter in
             broadcast ~raw_header ~raw_content network);
         loop (counter + 1)
       in *)
    let rec loop counter =
      let raw_header = Format.sprintf "sh%d" counter in
      broadcast ~raw_header ~raw_content network;

      Eio.Time.sleep clock 0.5;
      loop (counter + 1)
    in

    Eio.Fiber.all
      [
        (fun () ->
          listen ~net ~clock ~port ~on_connection ~on_request ~on_message
            network);
        (fun () ->
          connect ~net ~clock ~nodes ~on_connection ~on_request ~on_message
            network);
        (fun () -> loop 0);
      ]
  in

  let domains = Eio.Stdenv.domain_mgr env in
  let start ~port = Eio.Domain_manager.run domains (fun () -> start ~port) in
  Eio.Fiber.List.iter (fun (_host, port) -> start ~port) nodes
