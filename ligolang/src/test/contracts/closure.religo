/* Test whether closures retain values in ReasonLIGO */

let test = (k : int) : int => {
  let _j : int = k + 5;
  let close : (int => int) = (i : int) => i + _j;

  let _j : int = 20; /* Shadow original variable */
  close (20);
};
