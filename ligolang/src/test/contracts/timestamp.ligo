type storage_ is timestamp

function main (const p : unit; const s : storage_) :
  list (operation) * storage_ is ((nil: list (operation)), now)
