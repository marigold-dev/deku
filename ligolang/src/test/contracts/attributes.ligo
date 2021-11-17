[@annot] const x : int = 1;

[@inline] function foo (const a : int) : int is
  block {
    [@inline] const test : int = 2 + a;
  } with test;

[@inline][@other] const y : int = 1;

function bar (const b : int) : int is
  block {
    [@inline][@foo][@bar]
    function test (const z : int) : int is
      block {
        const r : int = 2 + b + z
      } with r;
  } with test (b)
