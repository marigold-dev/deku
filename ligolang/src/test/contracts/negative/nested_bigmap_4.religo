/* this should result in an error as nested big_maps are not supported: */
type storage = map (int, big_map (nat, big_map (int, string))); 

type return = (list (operation), storage);

let main = ((_, store): (unit, storage)): return  => {
    ([]: list(operation), store)
};
  