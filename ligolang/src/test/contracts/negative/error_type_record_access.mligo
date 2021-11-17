type foo = {
  i : int;
}

let bar (x : foo) : int = 
  let y : bool = x.i in
  42
