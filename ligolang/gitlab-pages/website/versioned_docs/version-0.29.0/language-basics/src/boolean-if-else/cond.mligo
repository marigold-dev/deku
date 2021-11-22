type magnitude = Small | Large // See variant types

let compare (n : nat) : magnitude =
  if n < 10n then Small else Large
