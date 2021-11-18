(* Parsing errors for the compiler *)

val stage : string

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Display  = Simple_utils.Display

(* Errors *)

type t = [`Parsing of string Region.reg]

type error = t

(* Colour snippet (Format) *)

type pp_formater =
  display_format:(string Display.display_format) ->
  Format.formatter ->
  t ->
  unit

val error_ppformat : pp_formater
val to_ppformat    : pp_formater (* Alias of [error_ppformat] *)

(* JSON *)

val error_jsonformat : t -> Yojson.Safe.t
val to_json : t -> Yojson.Safe.t  (* Alias of [error_jsonformat] *)
