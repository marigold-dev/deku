(* Let's inline `plus_one` and see if the contract
   gets smaller and consumes less gas *)

type a_complex_record is
  record [
    complex : int;
    object : int;
    that : int;
    has : int;
    many : int;
    fields : int;
    and_some : int;
    counter : int
  ]

[@inline] function plus_one (const r : a_complex_record) is
  r with
    record [counter = r.counter + 1]

function main (const p : int; const s : a_complex_record) is
  ((list [] : list (operation)), plus_one (plus_one (s)))
