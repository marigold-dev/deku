type passport is
  record [
    series : int;
    number : int
  ]

type user is
  record [
    id       : passport;
    is_admin : bool;
    name     : string
  ]

const alice : user =
  record [
    id       = record [ series = 0; number = 1 ];
    is_admin = True;
    name     = "Alice"
  ]

const alice_admin : int = alice.id.s