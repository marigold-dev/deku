(* This file provides an interface to the ReasonLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module CST    = Cst_reasonligo.CST
module Errors = Parsing_shared.Errors

(* Parsing *)

type file_path = string

(* All function read a string buffer but they differ in the way they
   interpret it: [from_file] assumes that its contents comes
   originally from a file, [from_string] assumes that its contents
   comes originally from a string, and [expression] assumes that is
   contents is an expression and comes from a string. *)

val from_file   : raise:Errors.t Trace.raise -> Buffer.t -> file_path -> CST.t
val from_string : raise:Errors.t Trace.raise -> Buffer.t -> CST.t
val expression  : raise:Errors.t Trace.raise -> Buffer.t -> CST.expr

(* Aliases *)

val parse_file       : raise:Errors.t Trace.raise -> Buffer.t -> file_path -> CST.t
val parse_string     : raise:Errors.t Trace.raise -> Buffer.t -> CST.t
val parse_expression : raise:Errors.t Trace.raise -> Buffer.t -> CST.expr

(* Pretty-printing *)

(* The function [pretty_print_file] reads a string buffer and assumes
   that its contents originally comes from a file. *)

val pretty_print            : CST.t -> Buffer.t
val pretty_print_expression : CST.expr -> Buffer.t
val pretty_print_pattern    : CST.pattern -> Buffer.t
val pretty_print_type_expr  : CST.type_expr -> Buffer.t
val pretty_print_file       : raise:Errors.t Trace.raise -> Buffer.t -> file_path -> Buffer.t
val pretty_print_cst        : raise:Errors.t Trace.raise -> Buffer.t -> file_path -> Buffer.t
