type foo is A | B

function t (const x: foo) is
  case x of
  | A -> "hey"
  | B -> 2
  end