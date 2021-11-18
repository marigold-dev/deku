function local_type (var u : unit) : int is block {
  type toto is int;
  function foo (const b : toto) : toto is b
} with titi
