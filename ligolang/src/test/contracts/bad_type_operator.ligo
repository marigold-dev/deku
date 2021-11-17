type parameter is unit

type binding is nat * nat
type storage is map (binding)

type return is list (operation) * storage

function main (const param : parameter; const store : storage) : return is
  ((nil : list (operation)), store)
