module Content : sig
  open Deku_consensus

  type content = private Content_block of Block.t
  type t = content [@@deriving yojson]

  val block : Block.t -> content
end

type response = private
  | Response of { hash : Response_hash.t; content : Content.t }

type t = response

type raw_response = private
  | Raw_response of { hash : Response_hash.t; raw_content : string }

type raw = raw_response

val encode : content:Content.t -> response * raw_response
val decode : raw_content:string -> (response * raw_response) option
