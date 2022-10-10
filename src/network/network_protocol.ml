let () = assert (Sys.word_size = 64)

module Connection = struct
  type connection =
    | Connection of { read_buf : Eio.Buf_read.t; write_buf : Eio.Buf_write.t }

  type t = connection

  let of_stream stream k =
    let max_size = Network_message.max_size in
    Eio.Buf_write.with_flow ~initial_size:max_size stream @@ fun write_buf ->
    let read_buf =
      Eio.Buf_read.of_flow ~initial_size:max_size ~max_size stream
    in
    k (Connection { read_buf; write_buf })

  let read connection =
    let (Connection { read_buf; write_buf = _ }) = connection in
    Network_message.read read_buf

  let write connection message =
    let (Connection { read_buf = _; write_buf }) = connection in
    Network_message.write write_buf message
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

  let connect ~net ~host ~port k =
    Eio.Switch.run @@ fun sw ->
    let address = get_first_addrinfo_ipv4 ~net ~host ~port in
    let stream = Eio.Net.connect ~sw net address in
    Connection.of_stream stream k
end

module Server = struct
  (* TODO: I just wrote this number down, have no idea if it makes sense, magic *)
  let backlog = 1024

  let listen ~net ~port ~on_error k =
    Eio.Switch.run @@ fun sw ->
    let interface = Eio.Net.Ipaddr.V4.any in
    let address = `Tcp (interface, port) in
    let socket = Eio.Net.listen ~backlog ~sw net address in
    let rec loop () =
      Eio.Net.accept_fork ~sw socket ~on_error (fun stream _sockaddr ->
          Connection.of_stream stream k);
      loop ()
    in
    loop ()
end

let test () =
  Eio_main.run @@ fun env ->
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
    Server.listen ~net ~port ~on_error:Deku_constants.async_on_error handler
  in
  let client () =
    Client.connect ~net ~host ~port @@ fun connection ->
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
