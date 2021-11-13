(* Parsing the command-line options *)

(* The signature [COMMENTS] specifies the kind of comments
   expected. Those fields do not correspond to CLI (command-line
   options), as they are for internal use.

   WARNING: The delimiters for comments must be chosen from the ones
   recognised by the scanning rules in [API.mll]. *)

module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end

(* The signature [S] (command-line interface) gathers the options
   given to the tool, following the GNU convention, and exports then
   as module fields. *)

module type S =
  sig
    include COMMENTS

    val input     : string option (* input file     *)
    val extension : string option (* file extension *)
    val dirs      : string list   (* -I             *)
    val show_pp   : bool          (* --show-pp      *)
    val offsets   : bool          (* neg --columns  *)

    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    ]

    val status : status
  end

(* The instantiation of functor [Make] reads the command line
   interface. *)

module Make (Comments: COMMENTS) : S
