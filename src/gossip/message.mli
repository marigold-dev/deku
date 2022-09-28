module Content : sig
  open Deku_concepts
  open Deku_protocol
  open Deku_consensus

  type content = private
    (* TODO: signing and hashing should not be happening on the consensus thread *)
    | Content_block of Block.t
    | Content_vote of { level : Level.t; vote : Verified_signature.t }
    | Content_operation of Operation.t
    | Content_accepted of { block : Block.t; votes : Verified_signature.t list }

  type t = content [@@deriving yojson]

  val block : Block.t -> content
  val vote : level:Level.t -> vote:Verified_signature.t -> content
  val operation : Operation.t -> content
  val accepted : block:Block.t -> votes:Verified_signature.t list -> content
end

type message = private
  | Message of { hash : Message_hash.t; content : Content.t }

type t = message

type raw_message = private
  | Raw_message of { hash : Message_hash.t; raw_content : string }

type raw = raw_message

val encode : content:Content.t -> message * raw_message
val decode : raw_content:string -> (message * raw_message) option
