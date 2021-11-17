function foo(var x : int) : int is
  block {
    function bar(const x : int) : int is x;
  } with bar(42)
