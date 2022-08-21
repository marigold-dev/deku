open Deku_stdlib
open Deku_concepts
open Deku_consensus
open Deku_chain
open Deku_network
open Deku_storage

module Parallel = struct
  let domains = 8

  let pool =
    let pool = lazy (Parallel.Pool.make ~domains) in
    fun () -> Lazy.force pool
end

module Node = struct
  type node =
    | Node of {
        chain : Chain.t;
        network : Network.t;
        (* TODO: weird, but for timeouts*)
        trigger_timeout : unit -> unit;
      }

  type t = node

  let make ~identity ~validators ~nodes ~trigger_timeout =
    (* TODO: is this pool () here correct? *)
    let pool = Parallel.pool () in
    let chain = Chain.make ~identity ~validators ~pool in
    let network = Network.make ~nodes in
    Node { chain; network; trigger_timeout }

  let dispatch_effect effect node =
    let (Node { chain; network; trigger_timeout }) = node in
    let network =
      match effect with
      | Chain.Trigger_timeout ->
          let () = trigger_timeout () in
          network
      | Chain.Broadcast_block { block } ->
          Network.broadcast_block ~block network
      | Chain.Broadcast_signature { signature } ->
          Network.broadcast_signature ~signature network
    in
    Node { chain; network; trigger_timeout }

  let dispatch_effects effects node =
    List.fold_left (fun node effect -> dispatch_effect effect node) node effects

  let incoming_message (type a) ~current ~(endpoint : a Endpoint.t) ~message
      node =
    let (Node { chain; network; trigger_timeout }) = node in
    let message, network =
      Network.incoming_message ~endpoint ~message network
    in
    let chain, effects =
      match message with
      | Some message -> (
          match endpoint with
          | Blocks -> Chain.incoming_block ~current ~block:message chain
          | Signatures ->
              Chain.incoming_signature ~current ~signature:message chain
          | Operations ->
              let chain = Chain.incoming_operation ~operation:message chain in
              (chain, []))
      | None -> (chain, [])
    in
    let node = Node { chain; network; trigger_timeout } in
    dispatch_effects effects node

  let incoming_timeout ~current node =
    let (Node { chain; network; trigger_timeout }) = node in
    let chain, effects = Chain.incoming_timeout ~current chain in
    let node = Node { chain; network; trigger_timeout } in
    dispatch_effects effects node
end

module Singleton : sig
  val get_state : unit -> Node.t
  val set_state : Node.t -> unit
  val initialize : Storage.t -> unit
end = struct
  type state = { mutable state : Node.t; mutable trigger : unit Lwt.u }

  let state = ref None

  let get_server () =
    match !state with
    | Some server -> server
    | None -> failwith "uninitialized state"

  let get_state () =
    let server = get_server () in
    server.state

  let set_state new_state =
    let server = get_server () in
    server.state <- new_state

  let rec setup_timeout server =
    let open Deku_constants in
    let trigger_promise, trigger_resolver = Lwt.wait () in
    server.trigger <- trigger_resolver;

    let%await () = Lwt.pick [ Lwt_unix.sleep block_timeout; trigger_promise ] in
    let current = Timestamp.of_float (Unix.gettimeofday ()) in
    server.state <- Node.incoming_timeout ~current server.state;
    setup_timeout server

  let initialize storage =
    let Storage.{ secret; initial_validators; nodes } = storage in
    let identity = Identity.make secret in

    let trigger_timeout_ref = ref (fun () -> ()) in
    let trigger_timeout () = !trigger_timeout_ref () in
    let node =
      Node.make ~identity ~validators:initial_validators ~nodes ~trigger_timeout
    in
    let server =
      let _, trigger = Lwt.wait () in
      { state = node; trigger }
    in
    let () =
      trigger_timeout_ref :=
        fun () ->
          let trigger = server.trigger in
          Lwt.wakeup_later trigger ()
    in
    let () = Lwt.async (fun () -> setup_timeout server) in
    state := Some server
end

module Server = struct
  open Piaf

  let internal_error error =
    let response = Response.or_internal_error (Error error) in
    Lwt.return response

  let error ~message status =
    let response = Response.of_string ~body:message status in
    Lwt.return response

  let with_endpoint Server.{ ctx = _; request } next =
    let path = request.target in
    let meth = request.meth in

    match Endpoint.of_string path with
    | Some endpoint -> (
        match meth with
        | `POST -> next Server.{ ctx = endpoint; request }
        | _ -> error ~message:"only POST is supported" `Method_not_allowed)
    | None -> error ~message:"unknown endpoint" `Not_found

  let with_body Server.{ ctx = endpoint; request } next =
    let open Lwt.Infix in
    let body = request.body in
    Body.to_string body >>= fun result ->
    match result with
    | Ok body -> next Server.{ ctx = (endpoint, body); request }
    | Error error -> internal_error error

  let apply Server.{ ctx = endpoint, message; request = _ } =
    let node = Singleton.get_state () in
    let current = Timestamp.of_float (Unix.gettimeofday ()) in
    let (Endpoint.Ex endpoint) = endpoint in
    let node = Node.incoming_message ~current ~endpoint ~message node in
    let () = Singleton.set_state node in
    let response = Piaf.Response.of_string ~body:"OK" `OK in
    Lwt.return response

  let handler context =
    (* TODO: weird usage of @@ *)
    with_endpoint context @@ fun context ->
    with_body context @@ fun context -> apply context

  let start port =
    let open Lwt.Infix in
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    Lwt.async (fun () ->
        (* TODO: piaf error_handler *)
        Lwt_io.establish_server_with_client_socket listen_address
          (Server.create ?config:None ?error_handler:None handler)
        >|= fun _server -> Printf.printf "Listening on port %i\n%!" port);
    let forever, _ = Lwt.wait () in
    forever
end

let main () =
  let port = ref 8080 in
  let storage = ref "storage.json" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Listening port number (8080 by default)");
      ("-s", Arg.Set_string storage, " Storage file (storage.json by default)");
    ]
    ignore "Handle Deku communication. Runs forever.";

  let open Lwt.Infix in
  Storage.read ~file:!storage >>= fun storage ->
  let () = Singleton.initialize storage in
  Server.start !port

let () = Lwt_main.run (main ())
