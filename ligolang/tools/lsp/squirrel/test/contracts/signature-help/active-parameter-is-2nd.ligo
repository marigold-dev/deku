function bar (const a : int; const b : int) : int is a + b

function foo (const i : int) : int is bar (i, i * 2)
