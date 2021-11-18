function foo(const x : int) : int is
  block {
    function bar(var x : int) : int is
      block { x := x + 1; } with x;
  } with bar(42)
