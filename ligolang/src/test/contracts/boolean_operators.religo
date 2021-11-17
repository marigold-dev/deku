// Test ReasonLIGO boolean operators

let or_true   = (b : bool) : bool => b || true;
let or_false  = (b : bool) : bool => b || false;
let and_true  = (b : bool) : bool => b && true;
let and_false = (b : bool) : bool => b && false;
let not_bool  = (b : bool) : bool => !b;
