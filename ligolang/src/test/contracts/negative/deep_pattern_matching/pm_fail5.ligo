type foo is None_fake | Some_fake of int

function t (const x: option(int)) is
  case x of
  | Some_fake (x) -> x
  | None_fake -> 1
  end