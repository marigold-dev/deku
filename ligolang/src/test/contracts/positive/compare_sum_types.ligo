type foo is Foo | Bar

function main (const p : foo; const _s : bool) : list(operation) * bool is
  ((nil : list (operation)), p = Foo)