type 'a foo = 'a * unit
type ('a,'b) bar = 'a foo * 'b

let t1 : int * string =
  let v : (int,string) bar = ((1,()),"one") in
  let ((x,()),y) = v in
  (x,y)

type 'a foo = 'a list
type bar = int foo

let t2 : int list =
  let f (x: int list) : bar =
    List.map (fun (i:int) -> i +1) x
  in
  let z : bar = [ 1 ; 2 ; 3 ] in
  f z

type 'b foo = 'b * int
type 'a bar = 'a foo
let t3 : nat bar = (1n,1)

type 'a foo = int list
let t4 : string foo = [ 1 ; 2 ; 3]