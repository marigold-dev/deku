open Deku_stdlib
open Deku_crypto

let () = assert (Sys.word_size = 64)

exception Invalid_handshake
exception Invalid_message_size

let max_size = 128 * 1024 * 1024

module Make_read_and_write (T : sig
  type t

  val encoding : t Data_encoding.t
end) =
struct
  open Data_encoding

  let encoding = check_size max_size T.encoding

  let read =
    let open Eio.Buf_read in
    let int32_size = 4 (* 4 bytes *) in
    let int32 buf =
      let size = take int32_size buf in
      let size = String.get_int32_le size 0 in
      let size = Int32.to_int size in
      size
    in
    let size buf ~max =
      let size = int32 buf in
      match size < 0 || size > max with
      | true -> raise Invalid_message_size
      | false -> size
    in
    fun buf ->
      let size = size ~max:max_size buf in
      let content = take size buf in
      Data_encoding.Binary.of_string_exn encoding content

  let write =
    let open Eio.Buf_write in
    let int32 buf size = LE.uint32 buf (Int32.of_int size) in

    fun buf t ->
      let payload = Binary.to_string_exn encoding t in
      let size = String.length payload in
      pause buf;
      int32 buf size;
      string buf payload;
      flush buf
end

module Connection = struct
  type connection =
    | Connection of {
        sw : Eio.Switch.t;
        owner : Key.t;
        read_stream : Network_message.t Eio.Stream.t;
        write_stream : Network_message.t Eio.Stream.t;
      }

  type t = connection

  let x = Yojson.equal

  module Handshake = struct
    module Challenge = Make_read_and_write (Network_handshake.Challenge)
    module Response = Make_read_and_write (Network_handshake.Response)
  end

  module Message = Make_read_and_write (Network_message)

  let owner connection =
    let (Connection { sw = _; owner; read_stream = _; write_stream = _ }) =
      connection
    in
    owner

  let read connection =
    let (Connection { sw; owner = _; read_stream; write_stream = _ }) =
      connection
    in
    Eio.Switch.check sw;
    Eio.Stream.take read_stream

  let write connection message =
    let (Connection { sw; owner = _; read_stream = _; write_stream }) =
      connection
    in
    Eio.Switch.check sw;
    Eio.Stream.add write_stream message

  let of_stream ~sw ~identity stream k =
    Eio.Buf_write.with_flow ~initial_size:max_size stream @@ fun write_buf ->
    let read_buf =
      Eio.Buf_read.of_flow ~initial_size:max_size ~max_size stream
    in

    (* handshake *)
    let sent_challenge = Network_handshake.Challenge.generate () in
    Handshake.Challenge.write write_buf sent_challenge;

    let recv_challenge = Handshake.Challenge.read read_buf in
    let sent_response =
      Network_handshake.Response.answer ~identity recv_challenge
    in
    Handshake.Response.write write_buf sent_response;

    let recv_response = Handshake.Response.read read_buf in
    (match
       Network_handshake.Response.verify ~challenge:sent_challenge recv_response
     with
    | true -> ()
    | false -> raise Invalid_handshake);

    (* connected *)
    let owner = Network_handshake.Response.key recv_response in
    let read_stream = Eio.Stream.create 0 in
    let write_stream = Eio.Stream.create 0 in

    let rec read_loop () : unit =
      Eio.Fiber.check ();
      let () =
        Parallel.parallel @@ fun () ->
        let message = Message.read read_buf in
        Eio.Stream.add read_stream message
      in
      read_loop ()
    in
    let rec write_loop () : unit =
      Eio.Fiber.check ();
      let () =
        Parallel.parallel @@ fun () ->
        let message = Eio.Stream.take write_stream in
        Message.write write_buf message
      in
      write_loop ()
    in
    Eio.Fiber.all
      [
        (fun () -> read_loop ());
        (fun () -> write_loop ());
        (fun () -> k (Connection { sw; owner; read_stream; write_stream }));
      ]
end

module Client = struct
  exception Invalid_host

  let get_first_addrinfo_ipv4 ~net ~host ~port =
    let service = string_of_int port in
    let address = Eio.Net.getaddrinfo_stream ~service net host in
    let address =
      List.filter
        (fun address ->
          match address with
          | `Unix _ -> false
          | `Tcp (ip, _port) ->
              Eio.Net.Ipaddr.fold ~v4:(fun _ -> true) ~v6:(fun _ -> false) ip)
        address
    in
    (* TODO: is choosing the first okay? *)
    match address with address :: _ -> address | [] -> raise Invalid_host

  let connect ~identity ~net ~host ~port k =
    Eio.Switch.run @@ fun sw ->
    let address = get_first_addrinfo_ipv4 ~net ~host ~port in
    let stream = Eio.Net.connect ~sw net address in
    Connection.of_stream ~identity stream k
end

module Server = struct
  (* TODO: I just wrote this number down, have no idea if it makes sense, magic *)
  let backlog = 1024

  let listen ~identity ~net ~port ~on_error k =
    Eio.Switch.run @@ fun sw ->
    let interface = Eio.Net.Ipaddr.V4.any in
    let address = `Tcp (interface, port) in
    let socket =
      Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog ~sw net address
    in
    let rec loop () =
      Eio.Net.accept_fork ~sw socket ~on_error (fun stream _sockaddr ->
          Eio.Switch.run @@ fun sw ->
          Connection.of_stream ~sw ~identity stream k);
      loop ()
    in
    loop ()
end

let test () =
  let open Deku_concepts in
  Eio_main.run @@ fun env ->
  let identity =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret
  in
  let net = Eio.Stdenv.net env in
  let domains = Eio.Stdenv.domain_mgr env in
  let on_request ~send ~raw_header ~raw_content =
    let header, time =
      match String.split_on_char ':' raw_header with
      | [ header; time ] -> (header, time)
      | _ -> assert false
    in
    let time = float_of_string time in
    let delta = Unix.gettimeofday () -. time in
    Format.eprintf "request.header: %s; latency: %.3f; fragments: %d;\n%!"
      header delta
      (String.length raw_content);
    send ~raw_header ~raw_content
  in
  let on_message ~raw_header ~raw_content =
    let header, time =
      match String.split_on_char ':' raw_header with
      | [ header; time ] -> (header, time)
      | _ -> assert false
    in
    let time = float_of_string time in
    let delta = Unix.gettimeofday () -. time in
    Format.eprintf "message.header: %s; latency: %.3f; content: %d\n%!" header
      delta
      (String.length raw_content)
  in

  let host = "localhost" in
  let port = 1235 in
  let handler connection =
    let send ~raw_header ~raw_content =
      let message = Network_message.message ~raw_header ~raw_content in
      Connection.write connection message
    in
    let on_message message =
      match message with
      | Network_message.Message { raw_header; raw_content } ->
          on_message ~raw_header ~raw_content
      | Network_message.Request { raw_header; raw_content } ->
          on_request ~send ~raw_header ~raw_content
    in
    let rec loop () =
      let message = Connection.read connection in
      on_message message;
      loop ()
    in
    loop ()
  in

  let server () =
    Server.listen ~identity ~net ~port ~on_error:Deku_constants.async_on_error
      handler
  in
  let client () =
    Client.connect ~identity ~net ~host ~port @@ fun connection ->
    let raw_content = String.make (100 * 1024 * 1024) 'a' in
    let rec loop_write counter =
      let _request () =
        let raw_header =
          Format.sprintf "rh%d:%f" counter (Unix.gettimeofday ())
        in
        let message = Network_message.request ~raw_header ~raw_content in
        Connection.write connection message
      in
      let message () =
        let raw_header =
          Format.sprintf "sh%d:%f" counter (Unix.gettimeofday ())
        in
        let message = Network_message.message ~raw_header ~raw_content in
        Connection.write connection message
      in
      message ();
      loop_write (counter + 1)
    in
    Eio.Fiber.both (fun () -> handler connection) (fun () -> loop_write 0)
  in
  Eio.Fiber.both
    (fun () -> Eio.Domain_manager.run domains server)
    (fun () -> Eio.Domain_manager.run domains client)
