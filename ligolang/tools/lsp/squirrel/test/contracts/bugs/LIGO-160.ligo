type q is record a : int; b : int end
type w is record c : int; d : q end
const foo : q = block {
  const x : q = record [ a = 1; b = 2 ] ;
  const y : w = record [ c = 5; d = x ] ;
} with
  y.d with record [ a = 5 ]  // The problem is in "y.d"
