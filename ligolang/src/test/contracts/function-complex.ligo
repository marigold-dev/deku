// Test a PascaLIGO function with more complex logic than function.ligo

function main (const i : int) : int is
  block {
    var _j : int := 0;
    var k : int := 1;
    _j := k + i;
    k := i + _j
  } with k + _j
