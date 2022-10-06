type message =
  | Message of { raw_header : string; raw_fragments : string list }
  | Request of { raw_header : string; raw_fragments : string list }

type t = message

let message ~raw_header ~raw_fragments = Message { raw_header; raw_fragments }
let request ~raw_header ~raw_fragments = Request { raw_header; raw_fragments }

(* communication *)
exception Invalid_message

(* TODO: max size *)
let max_header_size = 128 * 1024
let max_size = 128 * 1024 * 1024

(* TODO: this max_size doesn't account for some stuff like separators *)

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
    | _ -> raise Invalid_message

  let separator = char ':'

  let eol =
    let* () = char '\r' in
    let+ () = char '\n' in
    ()

  let int32 =
    let+ size = take int32_size in
    let size = String.get_int32_le size 0 in
    let size = Int32.to_int size in
    size

  let size ~max =
    let+ size = int32 in
    match size < 0 || size > max with
    | true -> raise Invalid_message
    | false -> size

  let fragment ~max =
    let* size = size ~max in
    let* () = separator in
    take size

  let header = fragment ~max:max_header_size

  let fragments =
    let* fragments = int32 in
    (* TODO: this seems very non idiomatic *)
    let rec loop acc remaining max buf =
      match remaining <= 0 with
      | true -> List.rev acc
      | false ->
          (match max <= 0 with true -> raise Invalid_message | false -> ());

          let () = separator buf in
          let fragment = fragment ~max buf in

          let acc = fragment :: acc in
          let remaining = remaining - 1 in
          let max = max - String.length fragment - 1 (* separator *) in
          loop acc remaining max buf
    in
    loop [] fragments max_size

  let message =
    (* tag:header_size:header:fragments:size:fragment\r\n *)
    let open Syntax in
    let* tag = tag in
    let* () = separator in
    let* raw_header = header in
    let* () = separator in
    let* raw_fragments = fragments in
    let* () = eol in
    let message =
      match tag with
      | Message -> Message { raw_header; raw_fragments }
      | Request -> Request { raw_header; raw_fragments }
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

  let write_int32 buf size = LE.uint32 buf (Int32.of_int size)

  let write_string buf s =
    (* TODO: check if content is not bigger than max_size? *)
    let size = String.length s in
    write_int32 buf size;
    write_separator buf;
    string buf s

  let write_header = write_string
  let write_fragment = write_string

  let write_fragments buf fragments =
    (* TODO: check if content is not bigger than max_size? *)
    write_int32 buf (List.length fragments);
    List.iter
      (fun s ->
        write_separator buf;
        write_fragment buf s)
      fragments

  let write_message buf ~tag ~raw_header ~raw_fragments =
    write_tag buf ~tag;
    write_separator buf;
    write_header buf raw_header;
    write_separator buf;
    write_fragments buf raw_fragments;
    write_eol buf;
    flush buf

  let message buf message =
    match message with
    | Message { raw_header; raw_fragments } ->
        write_message buf ~tag:Message ~raw_header ~raw_fragments
    | Request { raw_header; raw_fragments } ->
        write_message buf ~tag:Request ~raw_header ~raw_fragments
end

let read = Reader.message
let write = Writer.message
