open Deku_stdlib

type state

val state_encoding : state Data_encoding.t
val empty : state

module Joypad : sig
  type t = Down | Up | Left | Right | Start | Select | B | A
  [@@deriving eq, show]

  val to_string : t -> string
  val of_string : string -> t

  val cmdliner_converter :
    (string -> [> `Error of string | `Ok of t ])
    * (Format.formatter -> t -> unit)

  val encoding : t Data_encoding.t
end

type command =
  | Input of Joypad.t option
  | Input_and_advance of Joypad.t option * int

val init : Eio.Net.t -> unit
val send_input : command -> N.t
