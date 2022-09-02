type packet = private
  | Packet of { hash : Packet_hash.t; content : Yojson.Safe.t }

type t = packet [@@deriving yojson]

val make : content:Yojson.Safe.t -> packet
val verify : packet -> bool

val content_of_yojson :
  endpoint:'a Endpoint.post Endpoint.t -> Yojson.Safe.t -> 'a

val yojson_of_content :
  endpoint:'a Endpoint.post Endpoint.t -> 'a -> Yojson.Safe.t
