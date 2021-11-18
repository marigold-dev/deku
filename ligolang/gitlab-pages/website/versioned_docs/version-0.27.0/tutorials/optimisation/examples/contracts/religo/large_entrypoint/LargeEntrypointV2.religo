/* Let's see if we can benefit by moving a large entrypoint
   to lazy storage.
*/

/* Some large record for demonstration purposes */
type large_record = {
  a: string,
  b: int,
  c: bool,
  d: nat,
  e: (int => int),
  f: option(address)
};

/* An entrypoint that occupies a lot of space. Now it does not end up
   in the code of the contract: we put it to a big map upon origination;
   see /migrations/5_deploy_large_entrypoint.js */
let large_entrypoint = (p: int) => {
  let x = "A long line of meaningless words occupying storage";
  let some_lambda = (n: int) =>
    {
      a: "A large record with dummy values",
      b: n,
      c: true,
      d: 42,
      e: ((t: int) => t + 1),
      f: (None : option(address))
    };
  let fst = some_lambda(p);
  let snd = some_lambda(p + 2);
  (fst.b + snd.b)
};

let small_entrypoint = ((p: int) => p);

type parameter = LargeEntrypoint(int) | SmallEntrypoint(int);

type storage = {large_entrypoint: big_map(bool, (int => int)), result: int };

let load_large_ep = (storage: storage) => {
  let maybe_large_entrypoint: option(int => int) =
    Map.find_opt(true, storage.large_entrypoint);
  switch(maybe_large_entrypoint){
  | Some (ep) => ep
  | None => (failwith("Internal error") : (int => int))
  }
};

let main = ((parameter, storage): (parameter, storage)) => {
  let nop: list(operation) = [];
  switch(parameter){
  | LargeEntrypoint n =>
      (nop, {...storage, result: (load_large_ep(storage))(n)})
  | SmallEntrypoint n => (nop, {...storage, result: small_entrypoint(n)})
  }
};
