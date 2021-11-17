type foo is record [ a :int ; b : nat ]

const t1 = block {
  var record [ a = a ; f = b ] := record [ a = 1 ; b = 1n ] ;
} with (a,b)