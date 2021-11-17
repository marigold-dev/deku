(* Utility types and functions *)

(* Polymorphic identity function *)

val id : 'a -> 'a

(* Combinators *)

val ( <@ )  : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val swap    : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val lambda  : 'a -> 'b -> 'a
val curry   : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

(* Parametric rules for sequences

   nseq:    non-empty sequence;
   sepseq:  (possibly empty) sequence of separated items;
   nsepseq: non-empty sequence of separated items.
*)

type           'a nseq = 'a * 'a list
type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

(* Consing *)

val nseq_cons    : 'a -> 'a nseq -> 'a nseq
val nsepseq_cons : 'a -> 'sep -> ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_cons  : 'a -> 'sep -> ('a,'sep)  sepseq -> ('a,'sep) nsepseq

(* Reversing *)

val nseq_rev    : 'a nseq -> 'a nseq
val nsepseq_rev : ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_rev  : ('a,'sep)  sepseq -> ('a,'sep)  sepseq

(* Rightwards iterators *)

val nseq_foldl    : ('a -> 'b -> 'a) -> 'a ->        'b nseq -> 'a
val nsepseq_foldl : ('a -> 'b -> 'a) -> 'a -> ('b,'c) nsepseq -> 'a
val sepseq_foldl  : ('a -> 'b -> 'a) -> 'a -> ('b,'c)  sepseq -> 'a

val nseq_iter    : ('a -> unit) ->        'a nseq -> unit
val nsepseq_iter : ('a -> unit) -> ('a,'b) nsepseq -> unit
val sepseq_iter  : ('a -> unit) -> ('a,'b)  sepseq -> unit

(* Leftwards iterators *)

val nseq_foldr    : ('a -> 'b -> 'b) ->        'a nseq -> 'b -> 'b
val nsepseq_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsepseq -> 'b -> 'b
val sepseq_foldr  : ('a -> 'b -> 'b) -> ('a,'c)  sepseq -> 'b -> 'b

(* Maps *)

val nseq_map    : ('a -> 'b) -> 'a nseq -> 'b nseq
val nsepseq_map : ('a -> 'b) -> ('a,'c) nsepseq -> ('b,'c) nsepseq
val sepseq_map  : ('a -> 'b) -> ('a,'c)  sepseq -> ('b,'c)  sepseq

(* Conversions to lists *)

val nseq_to_list    :        'a nseq -> 'a list
val nsepseq_to_list : ('a,'b) nsepseq -> 'a list
val sepseq_to_list  : ('a,'b)  sepseq -> 'a list

(* Effectful symbol generator *)

val gen_sym : unit -> string

(* General tracing function *)

val trace : string -> out_channel option -> unit

(* Printing a string in red to standard error *)

val highlight : string -> unit

(* When failing to parse a specifed JSON format *)

val error_yojson_format : string -> ('a, string) result

(* Working with optional values *)

module Option :
  sig
    val apply     : ('a -> 'b) -> 'a option -> 'b option
    val rev_apply : ('a -> 'a) option -> 'a -> 'a
    val to_string : string option -> string
  end

(* An extension to the standard module [String] *)

module String :
  sig
    include module type of String
    module Map : Map.S with type key = t
    module Set : Set.S with type elt = t
  end

(* Integer maps *)

module Int :
  sig
    type t = int
    module Map : Map.S with type key = t
    module Set : Set.S with type elt = t
  end
