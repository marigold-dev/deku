// type foo = { a :int , b : nat }
// type bar = { c : int * nat ; d : foo }

let t1 =
  let (a,b,c,d) = (1,2,3,4) ;
  a

let t2 : string =
  let (a,(b,c),(d,(e,f))) = (1,(2,3),(4,(5,"7"))) ;
  f

let t3 =
  let (a,(b,(c,(d,e,f)))) = (1,(2,(3n,(4,5,"7")))) ;
  (a+b,c,f)

let t4 =
  let (a,(b,(c,(d,(e,f))),g)) = (1,(1n,(1,(1n,(1,1n))),1)) ;
  (a+c+e+g , b + d + f)

// let t5 : nat =
//   let { a = a , b = b } = { a : 1 , b : 1n } ;
//   b

// let t6 =
//   let x : foo =  { a = 1 ; b = 2n } in
//   let ({ a = a ; b = b },(c,d)) = (x,(1,1)) in
//   (a + c + d, b)

// let t7 =
//   let ( { c = (a,b) ; d = { a = c ; b = d } } ) = { c = ( 1 , 2n) ; d = { a = 1 ; b = 1n } } in
//   ( a + c , b + d )

// let t8 =
//   let (x, (y, { a = a ; b = b })) = (1, (1n, {a = 1 ; b = 1n})) in
//   (x + a , y + b)