function foo(const x : int) : int -> int is
  block {
    function bar(var y : int) : int is x + y;
  } with bar
