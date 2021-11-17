function bar (const i : int) : int is i + 1

function foo (const i : int) : int is
  block {
    var c : int := bar()
    c := c - 1
  } with c
