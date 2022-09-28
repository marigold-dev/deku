let () = assert (Sys.word_size = 64)

(* TODO: max size *)
let max_size = 16 * 1024 * 1024

(* TODO: I just wrote this number down, have no idea if it makes sense, magic *)
let backlog = 1024

module Tag = struct
  type tag = Request | Message
  type t = tag
end

module Reader : sig
  val message : (Tag.t * string * string) Eio.Buf_read.parser
end = struct
  open Eio.Buf_read
  open Syntax

  (* TODO: this could have preallocated buffers for each connection *)
  exception Invalid_tag
  exception Invalid_message_size

  (* TODO: magic numbers *)
  let tag_size = 7 (* request | message *)
  let int32_size = 4 (* 4 bytes *)

  let tag =
    let open Syntax in
    let+ tag = take tag_size in
    match tag with
    | "request" -> Tag.Request
    | "message" -> Tag.Message
    | _ -> raise Invalid_tag

  let separator = char ':'

  let eol =
    let* () = char '\r' in
    let+ () = char '\n' in
    ()

  let size =
    let+ size = take int32_size in
    let size = String.get_int32_le size 0 in
    let size = Int32.to_int size in
    match size < 0 || size > max_size with
    | true -> raise Invalid_message_size
    | false -> size

  let string =
    let* size = size in
    let* () = separator in
    take size

  let message =
    (* tag:size:hash:size:content\r\n *)
    let open Syntax in
    let* tag = tag in
    let* () = separator in
    let* raw_expected_hash = string in
    let* () = separator in
    let* raw_content = string in
    let* () = eol in
    return (tag, raw_expected_hash, raw_content)
end

module Writer : sig
  val write_message :
    Eio.Buf_write.t ->
    tag:Tag.t ->
    raw_expected_hash:string ->
    raw_content:string ->
    unit
end = struct
  open Eio.Buf_write

  let write_tag buf ~tag =
    match tag with
    | Tag.Request -> string buf "request"
    | Tag.Message -> string buf "message"

  let write_separator buf = char buf ':'

  let write_eol buf =
    char buf '\r';
    char buf '\n'

  let write_string buf s =
    (* TODO: check if content is not bigger than max_size? *)
    let size = String.length s in
    LE.uint32 buf (Int32.of_int size);
    write_separator buf;
    string buf s

  let write_message buf ~tag ~raw_expected_hash ~raw_content =
    write_tag buf ~tag;
    write_separator buf;
    write_string buf raw_expected_hash;
    write_separator buf;
    write_string buf raw_content;
    write_eol buf;
    flush buf
end

let on_connection ~sw ~on_request ~on_message stream k =
  (* TODO: what if error? h*)
  Eio.Buf_write.with_flow ~initial_size:max_size stream @@ fun write_buf ->
  let request ~raw_expected_hash ~raw_content =
    Eio.Fiber.fork ~sw @@ fun () ->
    Writer.write_message write_buf ~tag:Tag.Request ~raw_expected_hash
      ~raw_content
  in
  let send ~raw_expected_hash ~raw_content =
    Eio.Fiber.fork ~sw @@ fun () ->
    Writer.write_message write_buf ~tag:Tag.Message ~raw_expected_hash
      ~raw_content
  in
  let () = k ~request ~send in

  let read_buf = Eio.Buf_read.of_flow ~initial_size:max_size ~max_size stream in
  let seq = Eio.Buf_read.seq Reader.message read_buf in
  Seq.iter
    (fun (tag, raw_expected_hash, raw_content) ->
      match tag with
      | Tag.Request -> on_request ~send ~raw_expected_hash ~raw_content
      | Tag.Message -> on_message ~raw_expected_hash ~raw_content)
    seq

let listen ~sw ~net ~port ~on_error ~on_request ~on_message =
  let interface = Eio.Net.Ipaddr.V4.any in
  let socket = Eio.Net.listen ~backlog ~sw net (`Tcp (interface, port)) in
  let rec loop () =
    Eio.Net.accept_fork ~sw socket ~on_error (fun stream _sockaddr ->
        on_connection ~sw ~on_request ~on_message stream
        @@ fun ~request:_ ~send:_ -> ());
    loop ()
  in
  loop ()

exception Invalid_host

let connect ~sw ~net ~host ~port ~on_request ~on_message =
  let service = string_of_int port in
  let address = Eio.Net.getaddrinfo_stream ~service net host in
  let address =
    (* TODO: is choosing the first okay? *)
    match address with address :: _ -> address | [] -> raise Invalid_host
  in
  let flow = Eio.Net.connect ~sw net address in
  on_connection ~sw ~on_request ~on_message flow

let test () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let on_request ~send ~raw_expected_hash ~raw_content =
    Format.eprintf "request.hash: %s; content: %s\n%!" raw_expected_hash
      raw_content;
    send ~raw_expected_hash ~raw_content
  in
  let on_message ~raw_expected_hash ~raw_content =
    Format.eprintf "message.hash: %s; content: %s\n%!" raw_expected_hash
      raw_content
  in

  let host = "localhost" in
  let port = 1234 in
  let server () =
    listen ~sw ~net ~port ~on_error:Deku_constants.async_on_error ~on_request
      ~on_message
  in
  let client () =
    connect ~sw ~net ~host ~port ~on_request ~on_message
    @@ fun ~request ~send ->
    let rec loop counter =
      Eio.Fiber.both
        (fun () ->
          let raw_expected_hash = Format.sprintf "rh%d" counter in
          let raw_content = Format.sprintf "rc%d" counter in
          request ~raw_expected_hash ~raw_content)
        (fun () ->
          let raw_expected_hash = Format.sprintf "sh%d" counter in
          let raw_content = Format.sprintf "sc%d" counter in
          send ~raw_expected_hash ~raw_content);
      loop (counter + 1)
    in

    loop 0
  in
  Eio.Fiber.both (fun () -> server ()) (fun () -> client ())
