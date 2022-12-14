open Data_encoding

type message =
  (* TODO: cstruct here, zero copies, data encoding*)
  | Message of { raw_header : string; raw_content : string }
  | Request of { raw_header : string; raw_content : string }

type t = message

let message ~raw_header ~raw_content = Message { raw_header; raw_content }
let request ~raw_header ~raw_content = Request { raw_header; raw_content }

(* encoding *)

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
