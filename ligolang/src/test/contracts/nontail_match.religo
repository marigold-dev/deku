let a = {
  let z = 4;
  let opt : option(int) = Some(3);
  let i : int = switch (opt) {
  | Some (i) => i + 5
  | None => (failwith ("should be some >:(") : int)
  };
  i + z;
}
