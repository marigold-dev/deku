type sum_aggregator = {
  counter : int;
  sum : int
}

let counter (n : int) : int =
  let initial : sum_aggregator = {counter=0; sum=0} in
  let rec aggregate : sum_aggregator -> int = fun (prev: sum_aggregator) ->
    if prev.counter <= n then
      aggregate {counter = prev.counter + 1;
                   sum = prev.counter + prev.sum}
    else
      prev.sum
  in
  aggregate initial
