open Deku_gossip
open Network_protocol

type network = {
  mutable connection_id : Connection_id.t;
  (* TODO: Hashtbl would do a great job here *)
  mutable connections : (Network_message.message -> unit) Connection_id.Map.t;
}

type t = network

let make () =
  {
    connection_id = Connection_id.initial;
    connections = Connection_id.Map.empty;
  }

let create_connection network =
  let connection_id = network.connection_id in
  network.connection_id <- Connection_id.next connection_id;
  connection_id

let set_connection ~connection_id ~write network =
  network.connections <-
    Connection_id.Map.add connection_id write network.connections

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
      | Network_message.Message { raw_header; raw_fragments } ->
          on_message ~raw_header ~raw_fragments
      | Network_message.Request { raw_header; raw_fragments } ->
          on_request ~connection:connection_id ~raw_header ~raw_fragments
    in
    let rec loop () =
      let message = Connection.read connection in
      on_message message;
      loop ()
    in
    set_connection ~connection_id ~write network;
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
  let rec reconnect_loop ~net ~clock ~host ~port handler =
    try Network_protocol.Client.connect ~net ~host ~port handler
    with exn ->
      Format.eprintf "reconnect(%s:%d): %s\n%!" host port
        (Printexc.to_string exn);
      Eio.Time.sleep clock Deku_constants.reconnect_timeout;
      reconnect_loop ~net ~clock ~host ~port handler
  in
  with_connection ~on_connection ~on_request ~on_message network
  @@ fun handler -> reconnect_loop ~net ~clock ~host ~port handler

let listen ~net ~clock ~port ~on_connection ~on_request ~on_message network =
  let on_error exn =
    Format.eprintf "listen.connection: %s\n%!" (Printexc.to_string exn)
  in
  let rec relisten ~net ~clock ~port ~on_error handler =
    try Network_protocol.Server.listen ~net ~port ~on_error handler
    with exn ->
      Format.eprintf "relisten: %s\n%!" (Printexc.to_string exn);
      Eio.Time.sleep clock Deku_constants.listen_timeout;
      relisten ~net ~clock ~port ~on_error handler
  in
  let handler connection =
    with_connection ~on_connection ~on_request ~on_message network
    @@ fun handler -> handler connection
  in
  relisten ~net ~clock ~port ~on_error handler

let connect ~net ~clock ~nodes ~on_connection ~on_request ~on_message network =
  Eio.Fiber.List.iter
    (fun (host, port) ->
      try
        connect ~clock ~net ~host ~port ~on_connection ~on_request ~on_message
          network
      with exn ->
        Format.eprintf "connect(%s:%d): %s\n%!" host port
          (Printexc.to_string exn))
    nodes

let send ~message ~write =
  (* write always includes a fork *)
  try write message
  with exn -> Format.eprintf "write.error: %s\n%!" (Printexc.to_string exn)

let broadcast message network =
  Connection_id.Map.iter
    (fun _connection write -> send ~message ~write)
    network.connections

let request ~raw_header ~raw_fragments network =
  let request = Network_message.request ~raw_header ~raw_fragments in
  broadcast request network

let broadcast ~raw_header ~raw_fragments network =
  let message = Network_message.message ~raw_header ~raw_fragments in
  broadcast message network

let send_request ~connection ~raw_header ~raw_fragments network =
  match Connection_id.Map.find_opt connection network.connections with
  | Some write ->
      let message = Network_message.request ~raw_header ~raw_fragments in
      send ~message ~write
  | None ->
      (* dead connection *)
      ()

let send ~connection ~raw_header ~raw_fragments network =
  match Connection_id.Map.find_opt connection network.connections with
  | Some write ->
      let message = Network_message.message ~raw_header ~raw_fragments in
      send ~message ~write
  | None ->
      (* dead connection *)
      ()

let test () =
  Eio_main.run @@ fun env ->
  let nodes = [ ("localhost", 1234); ("localhost", 1235) ] in

  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  let start ~port : unit =
    let network = make () in
    let on_connection ~connection:_ = Format.eprintf "connected\n%!" in
    let on_request ~connection ~raw_header ~raw_fragments =
      Format.eprintf "request(%s:%.3f): %d\n%!" raw_header
        (Unix.gettimeofday ())
        (List.length raw_fragments);
      send ~connection ~raw_header ~raw_fragments network
    in
    let on_message ~raw_header ~raw_fragments =
      Format.eprintf "message(%s:%.3f): %d\n%!" raw_header
        (Unix.gettimeofday ())
        (List.length raw_fragments)
    in
    let raw_fragments = [ String.make 10_000_000 'a' ] in
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
      Format.eprintf "sending(%s:%.3f): %d\n%!" raw_header
        (Unix.gettimeofday ())
        (List.length raw_fragments);
      broadcast ~raw_header ~raw_fragments network;

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
