function foo(const toto : int) is
  block {
    var toto := 2;
    toto := 3;
  } with toto

function bar(var _u : unit) is
  block {
    const toto = 1;
    var toto := 2;
    toto := 3;
  } with toto
