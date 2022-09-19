module Content : sig
  open Deku_consensus

  type content = private Content_block of Block_hash.t
  type t = content [@@deriving yojson]

  val block : Block_hash.t -> content
end

type request = private
  | Request of { hash : Request_hash.t; content : Content.t }

type t = request

type raw_request = private
  | Raw_request of { hash : Request_hash.t; raw_content : string }

type raw = raw_request

val encode : content:Content.t -> request * raw_request
val decode : raw_content:string -> (request * raw_request) option
