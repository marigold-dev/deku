function foo(const x : int; var y : int) : int is
  block {
    x := 4;
    y := 3;
  } with x + y
