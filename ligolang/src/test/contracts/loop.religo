/* Test loops in ReasonLIGO */

let rec aux_simple = (i : int) : int =>
  if (i < 100) { aux_simple (i + 1); } else { i; };

let counter_simple = (n : int) : int => aux_simple (n);

type sum_aggregator = {
  counter : int,
  sum     : int,
};

let counter = (n : int) : int => {
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

let rec aux_nest = (prev : sum_aggregator) : sum_aggregator =>
  if (prev.counter < 100) {
    let sum : int =
      prev.sum + aux_simple (prev.counter);
    aux_nest ({counter: prev.counter + 1,
                  sum: sum});
  } else {
    ({counter: prev.counter, sum: prev.sum});
  };

let counter_nest = (_n : int) : int => {
  let initial : sum_aggregator = {counter: 0, sum: 0};
  let out : sum_aggregator = aux_nest (initial);
  out.sum;
};
