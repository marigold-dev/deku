
type parameter =
  Increment (int)
| Extend (never);

type storage = int;

let main = ((action, store) : (parameter, storage)) : (list (operation), storage) => {
  (([] : list (operation)),
   (switch (action) {
    | Increment (n) => store + n
    | Extend (k) => (Tezos.never(k) : storage)})
  )
};
