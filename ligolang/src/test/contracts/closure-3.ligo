// This might seem like it is covered by induction with closure-2.ligo,
// but it exists to prevent a regression on the bug patched by:
// https://gitlab.com/ligolang/ligo/commit/faf3bbc06106de98189f1c1673bd57e78351dc7e

function foobar (const i : int) : int is
  block {
    const j : int = 3;
    const k : int = 4;
    function add (const l : int) : int is i+j+k+l
  } with add (42)
