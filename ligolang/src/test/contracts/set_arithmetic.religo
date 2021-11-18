/* Test set operations in ReasonLIGO */

let literal_op = (_: unit) : set (string) =>
  Set.literal (["foo", "bar", "foobar"]);

let add_op = (s: set (string)) : set (string) =>
  Set.add ("foobar", s);

let remove_op = (s: set (string)) : set (string) =>
  Set.remove ("foobar", s);

let remove_deep = (s: (set (string), nat)): set (string) =>
  Set.remove ("foobar", s[0]);

let mem_op = (s: set (string)) : bool =>
  Set.mem ("foobar", s);

let size_op = (s: set (string)): nat =>
  Set.cardinal (s);
