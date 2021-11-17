// {x with a = 5} shouldn't typecheck; record update with number
// TODO: ligo crashes here so we can't extract location information
type recc = { a : int }

let my_func(): recc =
  let x = 5 in
  { x with a = 5 }
