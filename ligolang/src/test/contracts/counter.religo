type storage = int;

let main = ((p, s) : (int, storage)) : (list (operation), storage) =>
  ([] : list (operation), p + s);
