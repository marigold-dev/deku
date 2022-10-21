open Deku_concepts

module Network = struct
  type network =
    | Network_request of { raw_header : string; raw_content : string }

  type t = network

  let make ~raw_header ~raw_content =
    Network_request { raw_header; raw_content }
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
  let network = Network.make ~raw_header ~raw_content in
  Request { hash; above; network }

let decode ~expected ~raw_content =
  let json = Yojson.Safe.from_string raw_content in
  let above = Level.t_of_yojson json in
  let hash, raw_header, raw_content = hash ~above in
  let network = Network.make ~raw_header ~raw_content in
  (match Request_hash.equal expected hash with
  | true -> ()
  | false -> raise Expected_hash_mismatch);
  Request { hash; above; network }
