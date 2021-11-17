type action =
  | Increment(int)
  | Decrement(int);

let a : action = Increment (5);