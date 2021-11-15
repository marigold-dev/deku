type bar = big_map (nat, int);

/* this should result in an error as nested big_maps are not supported: */
type storage = big_map (int, bar); 

type return = (list (operation), storage);

let main = ((_, store): (unit, storage)): return  => {
    ([]: list(operation), store)
};
