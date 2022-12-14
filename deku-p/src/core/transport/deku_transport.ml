open Deku_crypto
open Deku_stdlib
open Deku_concepts
open Ed25519

module Frame_socket : sig
  type frame_socket
  type t = frame_socket

  val make :
    sw:Eio.Switch.t ->
    initial:'a ->
    on_frame:('a -> frame:Cstruct.t -> last:bool -> 'a) ->
    Eio.Net.stream_socket ->
    frame_socket

  val with_write : frame_socket -> (write:(Cstruct.t -> unit) -> 'a) -> 'a
end = struct
  let max_payload_length = 65535
  let max_frame_length = 2 (* uint16 *) + max_payload_length

  let rec read_loop source stream =
    let length =
      let length = Cstruct.create 2 in
      Eio.Flow.read_exact source length;
      Cstruct.BE.get_uint16 length 0
    in
    let payload = Cstruct.create length in
    Eio.Flow.read_exact source payload;
    (* backpressure *)
    Eio.Stream.add stream payload;
    read_loop source stream

  let rec write_loop sink stream =
    let rec take_all stream length =
      match length with
      | 0 -> []
      | _ ->
          let frame = Eio.Stream.take stream in
          frame :: take_all stream (length - 1)
    in
    let take_all stream =
      let length = Eio.Stream.length stream in
      take_all stream length
    in

    let frames = take_all stream in
    Eio.Flow.write sink frames;
    write_loop sink stream

  let rec on_frame_loop acc ~on_frame ~read_stream =
    let frame = Eio.Stream.take read_stream in
    let frame_length = Cstruct.length frame in
    let last = frame_length < max_payload_length in
    on_frame_loop ~on_frame ~read_stream @@ on_frame acc ~frame ~last

  type frame_socket = {
    switch : Eio.Switch.t;
    read_stream : Cstruct.t Eio.Stream.t;
    write_mutex : Eio.Mutex.t;
    write_stream : Cstruct.t Eio.Stream.t;
  }

  type t = frame_socket

  let make ~sw ~initial ~on_frame socket =
    (* TODO: magic number 65535 * 256 = 16mb; 65537 * 128 = 8mb + 8mb *)
    let read_stream = Eio.Stream.create 128 in
    let write_mutex = Eio.Mutex.create () in
    let write_stream = Eio.Stream.create 256 in
    Eio.Fiber.fork ~sw (fun () -> read_loop socket read_stream);
    Eio.Fiber.fork ~sw (fun () -> on_frame_loop initial ~on_frame ~read_stream);
    Eio.Fiber.fork ~sw (fun () -> write_loop socket write_stream);
    { switch = sw; read_stream; write_mutex; write_stream }

  let with_write socket f =
    Eio.Switch.check socket.switch;
    Eio.Mutex.lock socket.write_mutex;
    let fragment = ref Cstruct.empty in
    let write_frame buf =
      let buf_length = Cstruct.length buf in
      let length = Cstruct.create 2 in
      Cstruct.BE.set_uint16 length 0 buf_length;
      Eio.Stream.add socket.write_stream length;
      Eio.Stream.add socket.write_stream buf
    in
    let rec write_buf buf =
      let buf_length = Cstruct.length buf in
      match buf_length >= max_payload_length with
      | true ->
          let frame, remaining = Cstruct.split buf max_payload_length in
          write_frame frame;
          write_buf remaining
      | false -> fragment := buf
    in
    let write buf = write_buf (Cstruct.append !fragment buf) in
    try
      let value = f ~write in
      write_frame !fragment;
      Eio.Mutex.unlock socket.write_mutex;
      value
    with exn ->
      Eio.Switch.fail socket.switch exn;
      raise exn
end

module Transport_header_hash = struct
  open BLAKE2b

  type transport_header_hash = BLAKE2b.t
  type t = transport_header_hash

  include With_b58_and_encoding_and_yojson (struct
    (* TODO: valid prefix *)
    let name = "Transport_header_hash"
    let prefix = assert false
  end)
end

module Message_socket : sig
  type message_socket = Frame_socket.t
  type t = message_socket

  val make :
    sw:Eio.Switch.t ->
    on_message:
      (signature:Ed25519.Signature.signature ->
      hash:BLAKE2b.hash ->
      author:Ed25519.Key_hash.key_hash ->
      payload_length:int ->
      tag:int ->
      level:Level.level ->
      chunk:Cstruct.t ->
      last:bool ->
      unit) ->
    Eio.Net.stream_socket ->
    message_socket

  val with_message :
    message_socket ->
    signature:Ed25519.Signature.signature ->
    hash:BLAKE2b.hash ->
    author:Ed25519.Key_hash.key_hash ->
    payload_length:int ->
    tag:int ->
    level:Level.level ->
    (write:(Cstruct.t -> unit) -> 'a) ->
    'a
end = struct
  let max_payload_length = 256 * 1024 * 1024

  let header_encoding =
    let open Data_encoding in
    let payload_length =
      conv_with_guard
        (fun n -> Int32.of_int n)
        (fun n ->
          (* from Int32.unsigned_to_int *)
          let mask = 0xFFFFFFFF in
          let n = Int32.to_int n land mask in
          match n < max_payload_length with
          | true -> Ok n
          | false -> Error "message too big")
        int32
    in
    tup6 Signature.encoding Transport_header_hash.encoding Key_hash.encoding
      payload_length uint8 Level.encoding

  let header_length =
    match Data_encoding.Binary.fixed_length header_encoding with
    | Some length -> length
    | None -> failwith "header must be of fixed length"

  type message_socket = Frame_socket.t
  type t = message_socket

  type socket_state =
    | Pending
    | Streaming of { on_chunk : chunk:Cstruct.t -> last:bool -> unit }

  let make ~sw ~on_message socket =
    let on_message ~frame ~last =
      let header, payload = Cstruct.split frame header_length in
      let signature, hash, author, payload_length, tag, level =
        let header = Cstruct.to_string header in
        Data_encoding.Binary.of_string_exn header_encoding header
      in
      let on_chunk =
        on_message ~signature ~hash ~author ~payload_length ~tag ~level
      in
      on_chunk ~chunk:payload ~last;
      on_chunk
    in

    let on_frame state ~frame ~last =
      let state =
        match state with
        | Pending ->
            let on_chunk = on_message ~frame ~last in
            Streaming { on_chunk }
        | Streaming { on_chunk } ->
            on_chunk ~chunk:frame ~last;
            state
      in
      match last with true -> Pending | false -> state
    in
    Frame_socket.make ~sw ~initial:Pending ~on_frame socket

  let with_message socket ~signature ~hash ~author ~payload_length ~tag ~level f
      =
    Frame_socket.with_write socket @@ fun ~write ->
    let () =
      let header =
        Data_encoding.Binary.to_string_exn header_encoding
          (signature, hash, author, payload_length, tag, level)
      in
      let header = Cstruct.of_string header in
      write header
    in
    f ~write
end

module type S = sig
  val t : int
end
