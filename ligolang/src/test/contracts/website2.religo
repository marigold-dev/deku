/* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE */

type storage = int;

/* variant defining pseudo multi-entrypoint actions */

type parameter =
| Increment (int)
| Decrement (int);

let add = ((a,b): (int, int)): int => a + b;
let sub = ((a,b): (int, int)): int => a - b;

/* real entrypoint that re-routes the flow based on the parameter provided */

let main = ((p,storage): (parameter, storage)) => {
  let storage =
    switch (p) {
    | Increment(n) => add ((storage, n))
    | Decrement(n) => sub ((storage, n))
    };
  ([]: list (operation), storage);
};

/* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE */
