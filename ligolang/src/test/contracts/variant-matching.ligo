type foobar is
  Foo of int
| Bar of bool
| Kee of nat

function fb (const p : foobar) : int is
  case p of
    Foo (n) -> n
  | Bar (_) -> 42
  | Kee (_) -> 23
  end
