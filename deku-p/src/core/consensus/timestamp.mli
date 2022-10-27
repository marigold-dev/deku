(* TODO: probably should be under concepts *)
type timestamp
type t = timestamp [@@deriving show, yojson]

val encoding : timestamp Data_encoding.t
val genesis : timestamp
val of_float : float -> timestamp

(* TODO: remove this function *)
val to_float : timestamp -> float
(* operations *)
(* TODO: better naming, TLDR it measures how many skips since some time
   like for timeouts*)

val timeouts_since : current:timestamp -> since:timestamp -> int
val next_timeout : current:timestamp -> since:timestamp -> timestamp
