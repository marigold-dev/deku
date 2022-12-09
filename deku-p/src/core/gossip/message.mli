module Header : sig
  open Deku_concepts

  type header = private
    | Message_header of { hash : Message_hash.t; level : Level.t }

  type t = header [@@deriving show]

  val decode : raw_header:string -> header
end

module Content : sig
  open Deku_concepts
  open Deku_protocol
  open Deku_consensus

  exception Invalid_content

  type content = private
    | Content_block of Block.t
    | Content_vote of { level : Level.t; vote : Verified_signature.t }
    | Content_operation of Operation.Signed.t
    | Content_accepted of { block : Block.t; votes : Verified_signature.t list }

  type t = content [@@deriving show]

  val encoding : content Data_encoding.t
  val block : Block.t -> content
  val vote : level:Level.t -> vote:Verified_signature.t -> content
  val operation : Operation.Signed.t -> content
  val accepted : block:Block.t -> votes:Verified_signature.t list -> content
end

module Network : sig
  type network_message = private
    | Network_message of { raw_header : string; raw_content : string }

  type t = network_message

  val encoding : network_message Data_encoding.t
end

type message = private
  | Message of { header : Header.t; content : Content.t; network : Network.t }

type t = message

exception Expected_header_mismatch

val encode : content:Content.t -> message
val decode : expected:Header.t -> raw_content:string -> message
