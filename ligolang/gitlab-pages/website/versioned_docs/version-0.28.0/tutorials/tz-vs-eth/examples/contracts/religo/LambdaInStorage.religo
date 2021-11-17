type storage = {fn: option(int => int), value: int };

type parameter = CallFunction | SetFunction((int => int));

let call = ((fn, value): (option((int => int)), int)) => {
  switch(fn){
  | Some (f) => f(value)
  | None => (failwith("Lambda is not set") : int)
  }
};

let main = ((p, s): (parameter, storage)) => {
  let newStorage = 
    switch(p){
    | SetFunction fn => {...s, fn: Some (fn)}
    | CallFunction => {...s, value: call(s.fn, s.value)}
    };
  ([] : list(operation), newStorage)
};
