(* Purely functional queues based on a pair of lists *)

type 'a t = {rear: 'a list; front: 'a list}

let empty = {rear=[]; front=[]}

let enq x q = {q with rear = x::q.rear}

let rec deq = function
  {rear=[]; front=  []} -> None
| {rear;    front=  []} -> deq {rear=[]; front = List.rev rear}
| {rear;    front=x::f} -> Some ({rear; front=f}, x)

let rec peek = function
  {rear=[]; front=  []}      -> None
| {rear;    front=  []}      -> peek {rear=[]; front = List.rev rear}
| {rear=_;  front=x::_} as q -> Some (q,x)

let is_empty q = (q = empty)
