function bar (const i : int) : int is i + 1

function foo (const i : int) : int is
  block {
    const c : int = bar()
    var d := c - 1
  } with d
