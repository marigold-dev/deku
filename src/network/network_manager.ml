open Deku_gossip

type network = {
  mutable nodes :
    ((raw_expected_hash:string -> raw_content:string -> unit)
    * (raw_expected_hash:string -> raw_content:string -> unit))
    list;
  mutable next_request_id : Request_id.t;
  mutable requests :
    (raw_expected_hash:string -> raw_content:string -> unit) Request_id.Map.t;
}

type t = network

(* TODO: move to Deku_constants *)
let retry_timeout = 3.0
let drop ~raw_expected_hash:_ ~raw_content:_ = ()
let drop = (drop, drop)

let rec connect_loop ~sw ~clock ~net ~host ~port ~ref ~on_request ~on_message =
  let on_error = Deku_constants.async_on_error in
  try
    Network_protocol.connect ~sw ~net ~host ~port ~on_request ~on_message
    @@ fun ~request ~send -> Atomic.set ref (request, send)
  with exn ->
    Atomic.set ref drop;
    on_error exn;
    Eio.Time.sleep clock retry_timeout;
    connect_loop ~sw ~clock ~net ~host ~port ~ref ~on_request ~on_message

let rec listen_loop ~sw ~clock ~net ~port ~on_request ~on_message =
  let on_error = Deku_constants.async_on_error in
  try Network_protocol.listen ~sw ~net ~port ~on_error ~on_request ~on_message
  with exn ->
    on_error exn;
    Eio.Time.sleep clock retry_timeout;
    listen_loop ~sw ~clock ~net ~port ~on_request ~on_message

let make () =
  {
    nodes = [];
    next_request_id = Request_id.initial;
    requests = Request_id.Map.empty;
  }

let make_on_request ~on_request network ~send ~raw_expected_hash ~raw_content =
  let id = network.next_request_id in
  network.next_request_id <- Request_id.next id;
  network.requests <- Request_id.Map.add id send network.requests;
  on_request ~id ~raw_expected_hash ~raw_content

let listen ~sw ~env ~port ~on_request ~on_message network =
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let on_request = make_on_request ~on_request network in
  listen_loop ~sw ~clock ~net ~port ~on_request ~on_message

let connect ~sw ~env ~nodes ~on_request ~on_message network =
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let nodes_with_host =
    List.map
      (fun (host, port) ->
        let ref = Atomic.make drop in
        (host, port, ref))
      nodes
  in
  let nodes =
    List.map
      (fun (_host, _port, ref) ->
        let request ~raw_expected_hash ~raw_content =
          let request, _send = Atomic.get ref in
          request ~raw_expected_hash ~raw_content
        in
        let send ~raw_expected_hash ~raw_content =
          let _request, send = Atomic.get ref in
          send ~raw_expected_hash ~raw_content
        in
        (request, send))
      nodes_with_host
  in
  network.nodes <- nodes;
  let on_request = make_on_request ~on_request network in
  List.iter
    (fun (host, port, ref) ->
      Eio.Fiber.fork_sub ~sw ~on_error:Deku_constants.async_on_error
      @@ fun sw ->
      connect_loop ~sw ~clock ~net ~host ~port ~ref ~on_request ~on_message)
    nodes_with_host

let broadcast ~sw ~raw_expected_hash ~raw_content network =
  List.iter
    (fun (_request, send) ->
      Eio.Fiber.fork ~sw (fun () -> send ~raw_expected_hash ~raw_content))
    network.nodes

let request ~sw ~raw_expected_hash ~raw_content network =
  List.iter
    (fun (request, _send) ->
      Eio.Fiber.fork ~sw (fun () -> request ~raw_expected_hash ~raw_content))
    network.nodes

let respond ~id ~raw_expected_hash ~raw_content network =
  match Request_id.Map.find_opt id network.requests with
  | Some send ->
      network.requests <- Request_id.Map.remove id network.requests;
      Format.eprintf "response: %s\n%!" raw_expected_hash;
      (* TODO: Eio.Fiber.fork*)
      send ~raw_expected_hash ~raw_content
  | None ->
      (* TODO: what do I do here? *)
      Format.eprintf "duplicated respond\n%!";
      ()

let not_found ~id network =
  network.requests <- Request_id.Map.remove id network.requests

let test () =
  Eio_main.run @@ fun env ->
  let nodes =
    [
      ("localhost", 1234);
      ("localhost", 1235);
      ("localhost", 1236);
      ("localhost", 1236);
    ]
  in

  let start ~sw ~port : unit =
    let network = make () in
    let on_request ~id ~raw_expected_hash ~raw_content =
      Format.eprintf "request(%s): %s\n%!" raw_expected_hash raw_content;
      respond ~id ~raw_expected_hash ~raw_content network
    in
    let on_message ~raw_expected_hash ~raw_content =
      Format.eprintf "message(%s): %s\n%!" raw_expected_hash raw_content
    in

    let () =
      Eio.Fiber.fork ~sw (fun () ->
          listen ~sw ~env ~port ~on_request ~on_message network)
    in
    let () =
      Eio.Fiber.fork ~sw (fun () ->
          connect ~sw ~env ~nodes ~on_request ~on_message network)
    in

    let rec throughput_loop counter =
      Eio.Fiber.yield ();
      Eio.Fiber.both
        (fun () ->
          let raw_expected_hash = Format.sprintf "rh%d" counter in
          let raw_content = Format.sprintf "rc%d" counter in
          request ~sw ~raw_expected_hash ~raw_content network)
        (fun () ->
          let raw_expected_hash = Format.sprintf "sh%d" counter in
          let raw_content = Format.sprintf "sc%d" counter in
          broadcast ~sw ~raw_expected_hash ~raw_content network);
      throughput_loop (counter + 1)
    in
    let _ = throughput_loop in
    let clock = Eio.Stdenv.clock env in
    let rec latency_loop counter =
      Eio.Fiber.yield ();
      Eio.Time.sleep clock 0.01;
      let _request, send = List.nth network.nodes 0 in
      let raw_expected_hash = Format.sprintf "sh%d" counter in
      let raw_content =
        Format.sprintf "sc%d:%.3f" counter (Unix.gettimeofday ())
      in
      Format.printf "sending(%s): %s\n%!" raw_expected_hash raw_content;
      send ~raw_expected_hash ~raw_content;
      latency_loop (counter + 1)
    in

    latency_loop 0
  in

  let domains = Eio.Stdenv.domain_mgr env in
  let start ~port =
    Eio.Domain_manager.run domains (fun () ->
        Eio.Switch.run @@ fun sw -> start ~sw ~port)
  in
  Eio.Fiber.all (List.init 1 (fun n () -> start ~port:(1234 + n)))
(* let clock = Eio.Stdenv.clock env in *)
