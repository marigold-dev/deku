type message =
  | Message of { raw_expected_hash : string; raw_content : string }
  | Request of { raw_expected_hash : string; raw_content : string }

type t = message

let message ~raw_expected_hash ~raw_content =
  Message { raw_expected_hash; raw_content }

let request ~raw_expected_hash ~raw_content =
  Request { raw_expected_hash; raw_content }

(* communication *)
exception Invalid_tag
exception Invalid_message_size

(* TODO: max size *)
let max_size = 16 * 1024 * 1024

module Tag = struct
  type tag = Request | Message
end

module Reader : sig
  val message : Eio.Buf_read.t -> message
end = struct
  open Eio.Buf_read
  open Syntax

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
    let message =
      match tag with
      | Message -> Message { raw_expected_hash; raw_content }
      | Request -> Request { raw_expected_hash; raw_content }
    in
    return message
end

module Writer : sig
  val message : Eio.Buf_write.t -> message -> unit
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

  let message buf message =
    match message with
    | Message { raw_expected_hash; raw_content } ->
        write_message buf ~tag:Message ~raw_expected_hash ~raw_content
    | Request { raw_expected_hash; raw_content } ->
        write_message buf ~tag:Request ~raw_expected_hash ~raw_content
end

let read = Reader.message
let write = Writer.message
