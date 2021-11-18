type sum_aggregator is
  record [
    counter : int;
    sum : int
]

recursive function counter (const n : int) : int is
  block {
    const initial : sum_aggregator = record [ counter = 0; sum = 0 ];
    recursive function aggregate (const prev : sum_aggregator) : int is
      if prev.counter <= n
        then aggregate(prev with
          record [ counter = prev.counter + 1;
                   sum = prev.counter + prev.sum])
        else prev.sum
  } with aggregate(initial)