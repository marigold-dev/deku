module Content : sig
  open Deku_concepts

  type content = private Content_accepted of { above : Level.t }
  type t = content [@@deriving yojson]

  val accepted : above:Level.t -> content
end

type request = private
  | Request of { hash : Request_hash.t; content : Content.t }

type t = request

type raw_request = private
  | Raw_request of { hash : Request_hash.t; raw_content : string }

type raw = raw_request

val encode : content:Content.t -> request * raw_request
val decode : raw_content:string -> (request * raw_request) option
