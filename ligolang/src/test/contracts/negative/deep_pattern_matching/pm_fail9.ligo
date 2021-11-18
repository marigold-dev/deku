type myd is One of int | Two

function t (const x : myd) is
  case x of
  | One (a) -> 2
  | Two -> a
  end
