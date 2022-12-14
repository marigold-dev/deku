module Wrapped_item : sig
  type wrapped_item
  type t = wrapped_item

  type comb_wrapped_item = private
    | Comb_wrapped_item of {
        wrapped_item : wrapped_item;
        raw_operation : Protocol_operation.Raw.t;
      }

  (* TODO: encode could be avoided a lot of times,
      by wrapping directly from the binary stream *)
  val encode : Protocol_operation.Raw.t -> comb_wrapped_item option
  val decode : wrapped_item -> Protocol_operation.Raw.t option

  (* repr *)
  val encoding : wrapped_item Data_encoding.t
end

type payload = Protocol_operation.Raw.t option Seq.t
type t = payload

val encode : Wrapped_item.t list -> string
val decode : string -> (unit -> Wrapped_item.t) list
