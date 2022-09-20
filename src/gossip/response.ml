module Content = struct
  open Deku_consensus

  type content = Content_block of Block.t
  and t = content [@@deriving yojson]

  let block block = Content_block block
end

type response = Response of { hash : Response_hash.t; content : Content.t }
type t = response

type raw_response =
  | Raw_response of { hash : Response_hash.t; raw_content : string }

type raw = raw_response

let hash ~json_content =
  (* guarantees canonical representation *)
  let raw_content = Yojson.Safe.to_string json_content in
  let hash = Response_hash.hash raw_content in
  (hash, raw_content)

let encode ~content =
  let json_content = Content.yojson_of_t content in
  let hash, raw_content = hash ~json_content in
  let response = Response { hash; content } in
  let raw_response = Raw_response { hash; raw_content } in
  (response, raw_response)

let decode ~raw_content =
  try
    let json_content = Yojson.Safe.from_string raw_content in
    let hash, raw_content = hash ~json_content in
    let content = Content.t_of_yojson json_content in
    let response = Response { hash; content } in
    let raw_response = Raw_response { hash; raw_content } in
    Some (response, raw_response)
  with _exn -> None (* TODO: dump this exception *)
