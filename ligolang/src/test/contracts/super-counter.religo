type parameter =
  Increment (int)
| Decrement (int);

type storage = int;

type return = (list (operation), storage);

let main = ((action, store): (parameter, storage)) : return => {
  let store =
    switch (action) {
    | Increment (n) => store + n
    | Decrement (n) => store - n
    };
  ([] : list (operation), store);
};
