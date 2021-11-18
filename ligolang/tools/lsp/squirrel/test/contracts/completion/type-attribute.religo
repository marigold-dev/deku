type user = {
  id       : nat,
  is_admin : bool,
  name     : string
};

let alice : user = {
  id       : 1n,
  is_admin : true,
  name     : "Alice"
};

let alice_admin : bool = alice.i