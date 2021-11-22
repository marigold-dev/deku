(* Can we benefit from removing the function definition from
   the compiled Michelson code? First, let's look at the
   non-inlined version of a sample contract *)

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

(* A function that doesn't perform a lot of computations
   but has a complex type *)
function plus_one (const r : a_complex_record) is
  r with
    record [counter = r.counter + 1]

function main (const p : int; const s : a_complex_record) is
  ((list [] : list (operation)), plus_one (plus_one (s)))
