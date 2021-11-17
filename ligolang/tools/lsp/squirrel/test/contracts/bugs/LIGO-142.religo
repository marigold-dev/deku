// Can parse destructurings inside let definitions:
let t1 =
  let (a,b,c,d) = (1,2,3,4);
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

// Can parse let and type declarations in let definitions:
let e1 =
  let a = 1;
  type foo = { a : int , b : nat };
  let b : foo = { a : type bar = int; let aa : bar = 42; aa, b : 2n };
  let c = 2;
  let d =
    let x = 1;
    let y = 2;
    x * y;
  a + b.a + c + d

// Can parse declarations inside lambda:
let e2 = (x: int) => let y = 2; x - y

// Can parse declarations inside function applications (par):
let e3 = e2 (let a = (x: int) => 2 + x; (let f = a; f) (let arg = 40; arg))

// Can parse declarations inside a switch (par):
let e4 =
  switch (let a = true; a) {
  | true  => 1
  | false => 0
  }

// Can parse declarations after some op (par):
let e5 = (-(let a = 1; a)) * (let b = 2; b)