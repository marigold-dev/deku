// Test michelson insertion in PascaLIGO

function main (const p : nat; const s: nat ) : list (operation)* nat is block {
  const f : (nat * nat -> nat)= [%Michelson ({| ADD |} : nat *nat -> nat)];
} with ((nil: list(operation)), f (p, s))
