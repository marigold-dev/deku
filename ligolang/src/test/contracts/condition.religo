/* Test conditional in ReasonLIGO */

let simple = (i : int) => if (i == 2) { 42; } else { 0; };
let annot = (i : int) =>
  if (((i == 2) : bool)) { (42 : int); } else { (0 : int); };
let shadow = (i: int) => {
  let _result = 0;
  if (i == 2) {
    let _result = 42;
    _result;
  } else {
    let _result = 0;
    _result;
  };
};