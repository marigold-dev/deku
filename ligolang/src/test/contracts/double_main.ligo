type parameter is unit
type storage is int
type return is list (operation) * storage

function main(const p : parameter; const s : storage) : return is
  ((nil : list(operation)), s+1)

function main (const p : parameter; const s : storage) : return is
  block {
    const ret : return = main (p, s)
  } with (ret.0, ret.1 + 1)
