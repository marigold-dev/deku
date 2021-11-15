type myd is One of int | Two

function t (const x : myd) is
  case x of
  | One (1) -> 2
  | Two -> 1
  end
