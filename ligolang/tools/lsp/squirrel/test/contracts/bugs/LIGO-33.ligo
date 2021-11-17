const f : (nat -> int) =
  [%Michelson ({| { UNPAIR; ADD } |} : nat * nat -> nat)];
