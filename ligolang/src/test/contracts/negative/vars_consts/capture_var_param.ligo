function foo(var x : int) : int is
  block {
    function bar(const _ : unit) : int is x;
  } with bar
