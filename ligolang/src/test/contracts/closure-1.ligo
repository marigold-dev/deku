function foo (const i : int) : int is
  block {
    function add (const j : int) : int is i+j
  } with add (i)
