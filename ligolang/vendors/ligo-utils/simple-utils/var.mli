(* Currently, Var.t is equivalent to (string * int option). The
   optional counter value is present on variables generated with
   `fresh`.

   The intent is that there are two disjoint classes of variables:
   'user variables' (embedded with `of_name`) and 'generated
   variables' (generated with `fresh`.)

   Vars with indices are printed as %s#%d. This could be confusing if
   vars like `name_of "foo#121"` are allowed -- `name_of "foo#121"`
   will be _not equal_ to a generated var `fresh ~name:"foo"` with
   counter 121, despite being printed the same way.

   This module does not prevent that confusion. But, the LIGO lexer
   does not accept names like "foo#121" as possible variable names, so
   this confusion should not arise for us.  *)

type 'a t
val to_yojson : 'a t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> ('a t, string) Result.result

val equal : 'a t -> 'a t -> bool
val compare : 'a t -> 'a t -> int

(* Prints vars as %s or %s#%d *)
val pp : Format.formatter -> 'a t -> unit

(* Construct a user variable directly from a string. This should only
   be used for embedding user variable names. For programmatically
   generated variables, use `fresh`. Take care not to cause
   shadowing/capture except as the user intended. *)
val of_name : string -> 'a t

(* TODO don't use this, this should not exist. *)
val to_name : 'a t -> string

(* Generate a variable, using a counter value from a _global_
   counter. If the name is not provided, it will be empty. *)
val fresh : ?name:string -> unit -> 'a t

(* Generate a variable as with `fresh`, reusing the name part of the
   given variable. *)
val fresh_like : 'a t -> 'b t

(* Reset the global counter. Danger, do not use... Provided for tests
   only. *)
val reset_counter : unit -> unit

val debug : 'a t -> string

val is_generated : 'a t -> bool

val todo_cast : 'a t -> 'b t

(* Used for pretty-printing by the typer *)
type names_for_print = { get_name_for_print : 'a . 'a t -> string }
val with_names_for_print : names_for_print -> (unit -> unit) -> unit

val internal_get_name_and_counter : 'a t -> (string * int option)

val wildcard : string
