let () = assert (Sys.word_size = 64)

(* TODO: I just wrote this number down, have no idea if it makes sense, magic *)

let backlog = 1024

let on_connection stream handler =
  let open Network_message in
  Eio.Buf_write.with_flow ~initial_size:max_size stream @@ fun write_buf ->
  let read_buf = Eio.Buf_read.of_flow ~initial_size:max_size ~max_size stream in
  let read () = read read_buf in
  let write message = write write_buf message in
  handler ~read ~write

let listen ~net ~port ~on_error handler =
  Eio.Switch.run @@ fun sw ->
  let interface = Eio.Net.Ipaddr.V4.any in
  let socket = Eio.Net.listen ~backlog ~sw net (`Tcp (interface, port)) in
  let rec loop () =
    Eio.Net.accept_fork ~sw socket ~on_error (fun stream _sockaddr ->
        on_connection stream handler);
    loop ()
  in
  loop ()

exception Invalid_host

let connect ~net ~host ~port handler =
  Eio.Switch.run @@ fun sw ->
  let service = string_of_int port in
  let address = Eio.Net.getaddrinfo_stream ~service net host in
  let address =
    (* TODO: is choosing the first okay? *)
    match address with address :: _ -> address | [] -> raise Invalid_host
  in
  let flow = Eio.Net.connect ~sw net address in
  on_connection flow handler

let test () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let on_request ~send ~raw_header ~raw_content =
    Format.eprintf "request.header: %s; content: %s\n%!" raw_header raw_content;
    send ~raw_header ~raw_content
  in
  let on_message ~raw_header ~raw_content =
    Format.eprintf "message.header: %s; content: %s\n%!" raw_header raw_content
  in

  let host = "localhost" in
  let port = 1234 in
  let handler ~read ~write =
    let send ~raw_header ~raw_content =
      let message = Network_message.message ~raw_header ~raw_content in
      write message
    in
    let on_message message =
      match message with
      | Network_message.Message { raw_header; raw_content } ->
          on_message ~raw_header ~raw_content
      | Network_message.Request { raw_header; raw_content } ->
          on_request ~send ~raw_header ~raw_content
    in
    let rec loop () =
      let message = read () in
      on_message message;
      loop ()
    in
    loop ()
  in

  let server () =
    listen ~net ~port ~on_error:Deku_constants.async_on_error handler
  in
  let client () =
    connect ~net ~host ~port @@ fun ~read ~write ->
    let rec loop_write counter =
      Eio.Fiber.both
        (fun () ->
          let raw_header = Format.sprintf "rh%d" counter in
          let raw_content = Format.sprintf "rc%d" counter in
          let message = Network_message.request ~raw_header ~raw_content in
          write message)
        (fun () ->
          let raw_header = Format.sprintf "sh%d" counter in
          let raw_content = Format.sprintf "sc%d" counter in
          let message = Network_message.message ~raw_header ~raw_content in
          write message);
      loop_write (counter + 1)
    in
    Eio.Fiber.both (fun () -> handler ~read ~write) (fun () -> loop_write 0)
  in
  Eio.Fiber.both (fun () -> server ()) (fun () -> client ())
