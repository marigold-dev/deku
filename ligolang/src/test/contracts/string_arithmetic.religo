/* Test that the string concatenation syntax in ReasonLIGO works */

let length_op = (s: string) : nat => String.length (s);
let concat_op = (s: string) => s ++ "toto";
let sub_op = (s: string) : string => String.sub (1n, 2n, s);
