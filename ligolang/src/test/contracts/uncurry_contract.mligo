module Foo = struct
  let foo = ()
end

let foo (_x : unit) (_y : unit) (_z : unit) (_w : unit) : unit = ()

(* tests old type preservation bug w/ uncurrying a function call whose
   args also include a curried application *)
let mul (x : nat) (y : nat) = x * y
let mul_test (x : nat) (y : nat) (z : nat) : nat =
  mul x (mul y z)

let main (p, s : unit * unit) : operation list * unit =
  if mul_test 0n 1n 2n <> 0n
  then
    (failwith "impossible" : operation list * unit)
  else
    begin
      Foo.foo;
      foo p s p s;
      foo p s p s;
      (([] : operation list), ())
    end
