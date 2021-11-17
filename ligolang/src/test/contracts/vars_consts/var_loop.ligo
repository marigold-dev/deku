function foo(const x : int) : int is
  block {
    var i := 0;
    var b := 5;
    while i < x and b > 0 block {
      i := i + 1;
    }
  } with i;
