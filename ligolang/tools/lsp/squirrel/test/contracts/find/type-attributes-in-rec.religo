type sum_aggregator = {
  counter : int,
  sum     : int,
};

let rec counter = (n : int) : int => {
  let initial : sum_aggregator = {counter: 0, sum: 0};
  let rec aggregate = (prev : sum_aggregator):int =>
    if (prev.counter <= n) {
      aggregate ({counter : prev.counter + 1,
                    sum : prev.counter + prev.sum});
    } else {
      prev.sum;
    };
  aggregate (initial);
};
