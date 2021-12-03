/* Test use of multiple subroutines in a ReasonLIGO function */

let inc        = (i : int) : int => i + 1;
let double_inc = (i : int) : int => inc (i+1);
let foo        = (i : int) : int => inc (i) + double_inc (i);
