(* TODO: probably should be under concepts *)
type timestamp
type t = timestamp

(* operations *)
(* TODO: better naming, TLDR it measures how many skips since some time
   like for timeouts*)

val timeouts_since : current:timestamp -> since:timestamp -> int
