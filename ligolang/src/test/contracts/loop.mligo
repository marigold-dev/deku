(* Test functional iterators in CameLIGO *)

let rec aux_simple (i : int) : int =
  if i < 100 then aux_simple (i + 1) else i

let counter_simple (n : int) : int = aux_simple n

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

let rec aux_nest (prev : sum_aggregator) : int =
  if prev.counter < 100 then
    let sum = prev.sum + (aux_simple prev.counter) in
    aux_nest {counter = prev.counter + 1; sum = sum}
  else
    prev.sum

let counter_nest (_n : int) : int =
  let initial : sum_aggregator = {counter=0; sum=0} in
  let out = aux_nest initial
  in out
