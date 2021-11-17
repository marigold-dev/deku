function foo (const _i : int) : int is
  block {
    function bar (const _i : int) : int is _i
  } with bar (0)
