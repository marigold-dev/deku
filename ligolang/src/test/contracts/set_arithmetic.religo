/* Test set operations in ReasonLIGO */

let literal_op = (_: unit) : set (string) =>
  Set.literal (["foo", "bar", "foobar"]);

let size_op = (s: set (string)): nat =>
  Set.cardinal (s);

let add_op = (s: set (string)) : set (string) =>
  Set.add ("foobar", s);

let remove_op = (s: set (string)) : set (string) =>
  Set.remove ("foobar", s);

let remove_deep = (s: (set (string), nat)): (set (string),nat) =>
  (Set.remove ("foobar", s[0]),s[1]);

/* homogeneity with pascaligo */
let patch_op = (s: set (string)) : set (string) =>
  Set.add ("foobar", s);

let patch_op_deep = (s: (set (string), nat)) : (set (string), nat) =>
  (Set.add ("foobar", s[0]), s[1]);

let mem_op = (s: set (string)) : bool =>
  Set.mem ("foobar", s);

let iter_op = (s : set (int)) : int =>
  let _ = Set.iter ((_ : int) => (), s);
  0;

let aggregate = ( (i,j) : (list (int), int)) : list(int) => [j, ...i]

let fold_op  = (s : set (int)) : list(int) => Set.fold (aggregate, s, ([] : list(int)))

let aggregate = ( (i,j) : (int , list(int))) : list(int) => [i, ...j]
let fold_right = (s : set (int)) : list(int) => Set.fold_desc (aggregate,  s, ([] : list(int)))