type message = private
  | Message of { hash : Message_hash.t; content : Yojson.Safe.t }

type t = message [@@deriving yojson]

val make : content:Yojson.Safe.t -> message
val verify : message -> bool
val content_of_yojson : endpoint:'a Endpoint.t -> Yojson.Safe.t -> 'a
val yojson_of_content : endpoint:'a Endpoint.t -> 'a -> Yojson.Safe.t
