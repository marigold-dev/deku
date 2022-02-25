val make_encoding :
  name:string ->
  title:string ->
  to_string:('a -> string) ->
  of_string:(string -> 'a option) ->
  raw_encoding:'a Data_encoding.t ->
  'a Data_encoding.t

val parse_string_variant : (string -> 'a option) list -> string -> 'a option
  [@@ocaml.doc
    " [parse_string_variant of_string_list string] try to parse the string\n\
    \    using every of_string in the list, returns [Some 'a] when one matches "]

module Make_b58 : functor
  (H : sig
     type t
     val name : string
     val title : string
     val prefix : string
     val size : int
     val to_raw : t -> string
     val of_raw : string -> t option
   end)
  -> sig
  open H

  val size : int

  (* TODO: should this be exposed? *)
  val to_raw : t -> string
  (** [to_raw t] serialize t to a string containing raw bytes with no prefix *)

  val to_string : t -> string
    [@@ocaml.doc " [to_string t] encodeds t as a b58 string "]

  val of_string : string -> t option
    [@@ocaml.doc
      " [of_string string] is [Some t] if string contains a b58 of t "]

  val encoding : t Data_encoding.t
    [@@ocaml.doc " [encoding] is identical to Tezos PACK encoding "]
end
