module Content = struct
  open Deku_consensus

  type content = Content_block of Block_hash.t
  and t = content [@@deriving yojson]

  let block block = Content_block block
end

type request = Request of { hash : Request_hash.t; content : Content.t }
type t = request

type raw_request =
  | Raw_request of { hash : Request_hash.t; raw_content : string }

type raw = raw_request

let hash ~json_content =
  (* guarantees canonical representation *)
  let raw_content = Yojson.Safe.to_string json_content in
  let hash = Request_hash.hash raw_content in
  (hash, raw_content)

let encode ~content =
  let json_content = Content.yojson_of_t content in
  let hash, raw_content = hash ~json_content in
  let request = Request { hash; content } in
  let raw_request = Raw_request { hash; raw_content } in
  (request, raw_request)

let decode ~raw_content =
  try
    let json_content = Yojson.Safe.from_string raw_content in
    let hash, raw_content = hash ~json_content in
    let content = Content.t_of_yojson json_content in
    let request = Request { hash; content } in
    let raw_request = Raw_request { hash; raw_content } in
    Some (request, raw_request)
  with _exn -> None (* TODO: dump this exception *)
