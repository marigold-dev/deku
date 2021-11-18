
type parameter =
| Increment (int)
| Extend (never);

type storage = int;

let main = ((action,store): (parameter, storage)) => {
  let storage =
    switch (action) {
    | Increment (n) => store + n
    | Extend (k) => [%Michelson ({| { NEVER } |} : (never => int))](k)
    };
  ([]: list(operation), storage);
};
