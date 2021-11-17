type sum_aggregator = {
  counter : int;
  sum : int
}

let counter (n : int) : int =
  let initial : sum_aggregator = {counter=0; sum=0} in
  let a : int = initial.sum in
  a
