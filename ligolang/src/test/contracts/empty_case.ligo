type t is
  Bar of int
| Baz

function main (const x : t) : int is
  case x of
    Bar (n) -> n
  | Baz     -> -1
  end
