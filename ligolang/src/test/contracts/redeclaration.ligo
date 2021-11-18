function foo (const p : unit) : int is 0

function main (const p : unit; const s : int) : list (operation) * int is
  ((nil : list (operation)), foo (unit))

function foo (const p : unit) : int is 1
