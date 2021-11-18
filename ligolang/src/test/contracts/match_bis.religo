type storage = int;

type parameter =
  Increment (int)
| Decrement (int);

let add = ((a: int), (b: int)) => a + b;
let sub = ((a: int), (b: int)) => a - b;

let main = ((action, store) : (parameter, storage)) => {
  let store =
    switch (action) {
    | Increment (n) => add (store, n)
    | Decrement (n) => sub (store, n)
    };
  (([] : list (operation)), store);
};
