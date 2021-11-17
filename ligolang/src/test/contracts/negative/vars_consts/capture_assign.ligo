function foo(const _ : unit) is block {
  var x := 42;
  function bar(const _ : unit) is block {
    const y = 0;
    x := 6;
  } with unit;
  bar(unit);
} with (x);
