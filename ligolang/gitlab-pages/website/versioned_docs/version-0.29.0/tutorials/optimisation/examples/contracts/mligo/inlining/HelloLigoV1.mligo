(* Can we benefit from removing the function definition from
   the compiled Michelson code? First, let's look at the
   non-inlined version of a sample contract *)

type a_complex_record = {
  complex : int;
  record : int;
  that : int;
  has : int;
  many : int;
  fields : int;
  and_some : int;
  counter : int
}

(* A function that doesn't perform a lot of computations
   but has a complex type *)
let plus_one (r : a_complex_record) = {r with counter = r.counter + 1}

let main (p, s : int * a_complex_record) =
  ([] : operation list), plus_one (plus_one s)
