let test =
  let f = (fun (_ : (unit * unit)) -> ()) in
  Test.originate f () 0tez
