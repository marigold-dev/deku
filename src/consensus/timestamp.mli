(* TODO: probably should be under concepts *)
type timestamp
type t = timestamp

val of_float : float -> timestamp

(* TODO: remove this function *)
val to_float : timestamp -> float
(* operations *)
(* TODO: better naming, TLDR it measures how many skips since some time
   like for timeouts*)

val timeouts_since : current:timestamp -> since:timestamp -> int
