function toto (const i : int) : int is
  block {
    function tata (const j : int) : int is i+j;
    function titi (const j : int) : int is i+j
  } with tata (i) + titi (i)
