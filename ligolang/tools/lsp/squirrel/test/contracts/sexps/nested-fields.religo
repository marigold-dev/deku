type password = {
  series : int,
  number : int
};

type user = {
  id       : password,
  is_admin : bool,
  name     : string
};

let alice : user = {
  id       : { series : 0, number : 1 },
  is_admin : true,
  name     : "Alice"
};

let alice_admin : bool = alice.id.series