type foo = big_map(int, int);

let set2 = (n : int, m : foo) : foo => Big_map.update (23, Some (n), m);

let set_ = (x : (int, foo)) : foo => set2 (x[0], x[1]);

let add = ((n,m) : (int, foo)) : foo => Big_map.add (23, n, m);

let rm = (m : foo) : foo => Big_map.remove (42, m);

let gf = (m : foo) : int => Big_map.find (23, m);

let get = (m : foo) : option (int) => Big_map.find_opt (42, m);

let empty_map : foo = Big_map.empty;

let map1 : foo = Big_map.literal ([(23,0), (42,0)]);

let mutimaps = (m: foo, n: foo): foo => {
  let bar : foo = Big_map.update (42, Some (0), m);
  Big_map.update (42, get (bar), n);
};
