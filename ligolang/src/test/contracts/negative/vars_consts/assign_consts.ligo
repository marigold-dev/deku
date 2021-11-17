function foo(const z : int) : int is
  block {
    const (x, y) = (4, 5);
    x := 1;
  } with x + y + z
