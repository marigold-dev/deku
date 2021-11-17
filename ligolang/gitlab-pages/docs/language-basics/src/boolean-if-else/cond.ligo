type magnitude is Small | Large // See variant types

function compare (const n : nat) : magnitude is
  if n < 10n then Small else Large
