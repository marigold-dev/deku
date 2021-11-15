type bar = big_map (nat, int); 

type foo = {
    a: int,
    b: bar
};

/* this should result in an error as nested big_maps are not supported: */
type storage = big_map(nat, foo);

type return = (list (operation), storage);

let main = ((_, store): (unit, storage)): return  => {
    ([]: list(operation), store)
};
