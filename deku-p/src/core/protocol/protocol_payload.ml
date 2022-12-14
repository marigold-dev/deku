module Wrapped_item = struct
  type wrapped_item =
    | Wrapped_item of { wrapped : Cstruct.t; content : string }

  type t = wrapped_item

  type comb_wrapped_item =
    | Comb_wrapped_item of {
        wrapped_item : wrapped_item;
        raw_operation : Protocol_operation.Raw.t;
      }

  let len_size = 2
  let max_len = 65535

  let make_len ~content_len =
    match content_len <= max_len with
    | true ->
        let len_buf = Cstruct.create len_size in
        Cstruct.BE.set_uint16 len_buf 0 content_len;
        Some len_buf
    | false -> None

  let extract_pos buf ~off =
    let buf_len = String.length buf in
    match buf_len >= len_size with
    | true ->
        let len = String.get_uint16_be buf off in
        Some (off, len + len_size)
    | false -> None

  let extract string ~off ~len =
    let wrapped = Cstruct.create len in
    Cstruct.blit_from_string string off wrapped 0 len;
    let content = String.sub string (off + len_size) (len - len_size) in
    Wrapped_item { wrapped; content }

  let wrap ~content =
    let content_len = String.length content in
    match make_len ~content_len with
    | Some len_buf ->
        let wrapped =
          (* TODO: this is not efficient *)
          let content_buf = Cstruct.of_string content in
          Cstruct.append len_buf content_buf
        in
        Some (Wrapped_item { wrapped; content })
    | None -> None

  let encode raw_operation =
    match
      Data_encoding.Binary.to_string_opt Protocol_operation.Raw.encoding
        raw_operation
    with
    | Some content -> (
        match wrap ~content with
        | Some wrapped_item ->
            Some (Comb_wrapped_item { wrapped_item; raw_operation })
        | None -> None)
    | None -> None

  let decode wrapped_item =
    let (Wrapped_item { wrapped = _; content }) = wrapped_item in
    match
      Data_encoding.Binary.of_string_opt Protocol_operation.Raw.encoding content
    with
    | Some raw_operation -> Some raw_operation
    | None -> None

  let wrapped wrapped_item =
    let (Wrapped_item { wrapped; content = _ }) = wrapped_item in
    wrapped

  let encoding =
    let open Data_encoding in
    conv_with_guard
      (fun (Wrapped_item { wrapped = _; content }) -> content)
      (fun content ->
        match wrap ~content with
        | Some wrapped_item -> Ok wrapped_item
        | None -> Error "invalid content length")
      string
end

type payload = Protocol_operation.Raw.t option Seq.t
type t = payload

let encode payload =
  let payload = List.map Wrapped_item.wrapped payload in
  (* TODO: could be done in parallel *)
  Cstruct.to_string (Cstruct.concat payload)

let decode payload =
  let rec unfold acc off =
    match Wrapped_item.extract_pos payload ~off with
    | Some (from, len) ->
        let off = from + len in
        let thunk () = Wrapped_item.extract payload ~off ~len in
        unfold (thunk :: acc) off
    | None | (exception _) ->
        (* TODO: do something with this exception? *)
        (* TODO: all malformed data is ignored *)
        List.rev acc
  in
  unfold [] 0
