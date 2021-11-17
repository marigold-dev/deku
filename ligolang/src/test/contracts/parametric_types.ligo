type foo(a) is a * unit
type bar(a,b) is foo(a) * b

const t1 : int * string =
  block {
    const v : bar(int,string) = ((1,Unit),"one") ;
    const ((x,Unit),y) = v ;
  } with (x,y)

type foo(a) is list(a)
type bar is foo(int)

const t2 : list(int) =
  block {
    function f (const x: list(int)) : bar is List.map((function (const i:int) is i +1), x) ;
    const z : bar = list [ 1 ; 2 ; 3] ;
  } with f(z)

type foo(b) is b * int
type bar(a) is foo(a)
const t3 : bar(nat) = (1n,1)

type foo(a) is list(int)
const t4 : foo(string) = list [ 1 ; 2 ; 3]