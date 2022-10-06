open Deku_concepts

module Network = struct
  type network =
    | Network_request of { raw_header : string; raw_fragments : string list }

  type t = network

  let make ~raw_header ~raw_fragments =
    Network_request { raw_header; raw_fragments }
end

type request =
  | Request of { hash : Request_hash.t; above : Level.t; network : Network.t }

type t = request

let hash ~above =
  (* guarantees canonical representation *)
  let json = Level.yojson_of_t above in
  let raw_content = Yojson.Safe.to_string json in
  let hash = Request_hash.hash raw_content in
  let raw_header = Request_hash.to_b58 hash in
  (hash, raw_header, raw_content)

exception Expected_hash_mismatch
exception Invalid_message

let encode ~above =
  let hash, raw_header, raw_content = hash ~above in
  let raw_fragments = [ raw_content ] in
  let network = Network.make ~raw_header ~raw_fragments in
  Request { hash; above; network }

let decode ~expected ~raw_fragments =
  let raw_content =
    match raw_fragments with
    | [ raw_content ] -> raw_content
    | _ -> raise Invalid_message
  in
  let json = Yojson.Safe.from_string raw_content in
  let above = Level.t_of_yojson json in
  let hash, raw_header, raw_content = hash ~above in
  let raw_fragments = [ raw_content ] in
  let network = Network.make ~raw_header ~raw_fragments in
  (match Request_hash.equal expected hash with
  | true -> ()
  | false -> raise Expected_hash_mismatch);
  Request { hash; above; network }
