open Data_encoding

type message =
  | Message of { raw_header : string; raw_content : string }
  | Request of { raw_header : string; raw_content : string }

type t = message

let message ~raw_header ~raw_content = Message { raw_header; raw_content }
let request ~raw_header ~raw_content = Request { raw_header; raw_content }

(* encoding *)

let max_size = 128 * 1024 * 1024

let encoding =
  let header = dynamic_size ~kind:`Uint8 Variable.string in
  let content = dynamic_size ~kind:`Uint30 Variable.string in
  union ~tag_size:`Uint8
    [
      case ~title:"message" (Tag 0) (tup2 header content)
        (fun message ->
          match message with
          | Request _ -> None
          | Message { raw_header; raw_content } -> Some (raw_header, raw_content))
        (fun (raw_header, raw_content) -> Message { raw_header; raw_content });
      case ~title:"request" (Tag 1) (tup2 header content)
        (fun message ->
          match message with
          | Request { raw_header; raw_content } -> Some (raw_header, raw_content)
          | Message _ -> None)
        (fun (raw_header, raw_content) -> Request { raw_header; raw_content });
    ]

(* communication *)
exception Invalid_message_size

module Reader : sig
  val message : Eio.Buf_read.t -> message
end = struct
  open Eio.Buf_read
  open Syntax

  let int32_size = 4 (* 4 bytes *)

  let int32 =
    let+ size = take int32_size in
    let size = String.get_int32_le size 0 in
    let size = Int32.to_int size in
    size

  let size ~max =
    let+ size = int32 in
    match size < 0 || size > max with
    | true -> raise Invalid_message_size
    | false -> size

  let message buf =
    let size = size ~max:max_size buf in
    let content = take size buf in
    Data_encoding.Binary.of_string_exn encoding content
end

module Writer : sig
  val message : Eio.Buf_write.t -> message -> unit
end = struct
  open Eio.Buf_write

  let int32 buf size = LE.uint32 buf (Int32.of_int size)

  let message buf message =
    let payload = Binary.to_string_exn encoding message in
    let size = String.length payload in
    int32 buf size;
    string buf payload;
    flush buf
end

let read = Reader.message
let write = Writer.message
