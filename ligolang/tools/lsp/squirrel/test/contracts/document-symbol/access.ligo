// Example from https://ligolang.org/docs/advanced/entrypoints-contracts/#access-control

const owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);

function main (const action : parameter; const store : storage) : return is
  if Tezos.source =/= owner then (failwith ("Access denied.") : return)
  else ((nil : list (operation)), store)
