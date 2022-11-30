type t

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t

type frame_buffer = [ `Black | `Dark_gray | `Light_gray | `White ] array array
[@@deriving ord, eq, yojson, show]

val show : t -> string
val empty : t
val init : rom_path:string -> t
val advance : t -> frame_buffer

module Joypad : sig
  type t = Camlboy_lib.Joypad.key =
    | Down
    | Up
    | Left
    | Right
    | Start
    | Select
    | B
    | A
  [@@deriving show, yojson, eq]

  val cmdliner_converter :
    (string -> [> `Ok of t | `Error of string ])
    * (Format.formatter -> t -> unit)

  val encoding : t Data_encoding.t
end

val send_input : Joypad.t -> t -> unit
