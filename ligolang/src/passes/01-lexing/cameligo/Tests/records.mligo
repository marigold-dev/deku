type myrec = {foo : int ; bar : int}

let a = 1

// record
let b = {foo=(let i = 1 in let j = 2 in a+i+j) ; bar=24}

// record accessor
// let c = (let d = 1 in b).bar

// record update
// let e =
//   {(let f = 2 in b) with bar=(let g = a in g);}

let e =
  {b with bar=(let g = a in g + b.bar + a);}
