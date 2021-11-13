// Test michelson insertion in PascaLIGO

function michelson_add (var n : nat * nat ) : nat is block {
  const f : (nat * nat -> nat)= [%Michelson ({| { UNPAIR; ADD } |} : nat *nat -> nat)];
} with f (n)
