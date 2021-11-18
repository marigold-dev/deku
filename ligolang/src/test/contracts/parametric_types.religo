type foo('a) = ('a, unit);
type bar('a, 'b) = (foo('a), 'b);

let t1: (int, string) = 
  let v: bar(int, string) = ((1, ()), "one");
  let ((x,()),y) = v ;
  (x,y)

type foo('a) = list('a);
type bar = foo(int);

let t2: list(int) = 
  let f = (x: list(int)) : bar => 
    List.map((i: int) => i + 1 , x) ;
  let z: bar = [1, 2, 3] ;
  f(z)

type foo('b) = ('b, int);
type bar('a) = foo('a)
let t3 : bar(nat) = (1n,1)

type foo('a) = list(int)
let t4 : foo(string) = [ 1, 2, 3]