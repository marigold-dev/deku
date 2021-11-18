type storage = int;

type parameter =
| Add (int)
| Sub (int);

type return = (list (operation), storage);

let main = ((action, store): (parameter, storage)) => {
  let store =
    store +
      (switch (action) {
      | Add (n) => n
      | Sub (n) => -n
      });
  (([]: list (operation)), store);
};
