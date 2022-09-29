open Deku_gossip

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

let set_connection ~connection ~write network =
  network.connections <-
    Connection_id.Map.add connection write network.connections

let close_connection ~connection network =
  network.connections <- Connection_id.Map.remove connection network.connections

let with_connection ~on_request ~on_message network k =
  let connection = create_connection network in
  let handler ~sw ~read ~write =
    let write message = Eio.Fiber.fork ~sw @@ fun () -> write message in
    set_connection ~connection ~write network;

    let on_message message =
      Eio.Fiber.fork ~sw @@ fun () ->
      match message with
      | Network_message.Message { raw_expected_hash; raw_content } ->
          on_message ~raw_expected_hash ~raw_content
      | Network_message.Request { raw_expected_hash; raw_content } ->
          on_request ~connection ~raw_expected_hash ~raw_content
    in
    let rec loop () =
      let message = read () in
      on_message message;
      loop ()
    in
    loop ()
  in
  let handler ~read ~write =
    try Eio.Switch.run @@ fun sw -> handler ~sw ~read ~write
    with exn ->
      close_connection ~connection network;
      (* TODO: reraise *)
      raise exn
  in
  k ~handler

let connect ~net ~clock ~host ~port ~on_request ~on_message network =
  let rec reconnect_loop ~net ~clock ~host ~port ~handler =
    try
      Eio.Switch.run @@ fun sw ->
      Network_protocol.connect ~sw ~net ~host ~port handler
    with exn ->
      Format.eprintf "reconnect(%s:%d): %s\n%!" host port
        (Printexc.to_string exn);
      Eio.Time.sleep clock Deku_constants.reconnect_timeout;
      reconnect_loop ~net ~clock ~host ~port ~handler
  in
  with_connection ~on_request ~on_message network @@ fun ~handler ->
  reconnect_loop ~net ~clock ~host ~port ~handler

let listen ~net ~clock ~port ~on_request ~on_message network =
  let on_error exn =
    Format.eprintf "listen.connection: %s\n%!" (Printexc.to_string exn)
  in
  let rec relisten ~net ~clock ~port ~on_error handler =
    try Network_protocol.listen ~net ~port ~on_error handler
    with exn ->
      Format.eprintf "relisten: %s\n%!" (Printexc.to_string exn);
      Eio.Time.sleep clock Deku_constants.listen_timeout;
      relisten ~net ~clock ~port ~on_error handler
  in
  let handler ~read ~write =
    with_connection ~on_request ~on_message network @@ fun ~handler ->
    handler ~read ~write
  in
  relisten ~net ~clock ~port ~on_error handler

let connect ~net ~clock ~nodes ~on_request ~on_message network =
  Eio.Fiber.List.iter
    (fun (host, port) ->
      try connect ~clock ~net ~host ~port ~on_request ~on_message network
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

let request ~raw_expected_hash ~raw_content network =
  let request = Network_message.request ~raw_expected_hash ~raw_content in
  broadcast request network

let broadcast ~raw_expected_hash ~raw_content network =
  let message = Network_message.message ~raw_expected_hash ~raw_content in
  broadcast message network

let send ~connection ~raw_expected_hash ~raw_content network =
  match Connection_id.Map.find_opt connection network.connections with
  | Some write ->
      let message = Network_message.message ~raw_expected_hash ~raw_content in
      send ~message ~write
  | None ->
      (* dead connection *)
      ()

let test () =
  Eio_main.run @@ fun env ->
  let nodes = [ ("localhost", 1234) ] in

  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  let start ~port : unit =
    let network = make () in
    let on_request ~connection ~raw_expected_hash ~raw_content =
      Format.eprintf "request(%s): %s\n%!" raw_expected_hash raw_content;
      send ~connection ~raw_expected_hash ~raw_content network
    in
    let on_message ~raw_expected_hash ~raw_content =
      Format.eprintf "message(%s): %s\n%!" raw_expected_hash raw_content
    in

    let rec loop counter =
      Eio.Fiber.yield ();
      Eio.Fiber.both
        (fun () ->
          let raw_expected_hash = Format.sprintf "rh%d" counter in
          let raw_content = Format.sprintf "rc%d" counter in
          request ~raw_expected_hash ~raw_content network)
        (fun () ->
          let raw_expected_hash = Format.sprintf "sh%d" counter in
          let raw_content = Format.sprintf "sc%d" counter in
          broadcast ~raw_expected_hash ~raw_content network);
      loop (counter + 1)
    in
    Eio.Fiber.all
      [
        (fun () -> listen ~net ~clock ~port ~on_request ~on_message network);
        (fun () -> connect ~net ~clock ~nodes ~on_request ~on_message network);
        (fun () -> loop 0);
      ]
  in

  let domains = Eio.Stdenv.domain_mgr env in
  let start ~port = Eio.Domain_manager.run domains (fun () -> start ~port) in
  Eio.Fiber.List.iter (fun (_host, port) -> start ~port) nodes
