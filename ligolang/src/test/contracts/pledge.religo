/* Pledge-Distribute — Accept money from a number of contributors and then donate
   to an address designated by an oracle */

/* A lot of people (myself included) seem to expect an oracle to be more than it is.
   That is, they expect it to be something complicated when it's actually pretty simple.
   An oracle is just an authorized source of information external to the chain, like an
   arbiter or moderator. For example, it's not possible to do an HTTP request to get
   info from a weather site directly using a smart contract. So instead what you
   do is make (or use) an oracle service which uploads the data to the chain so
   that contracts can use it.
*/

type storage = address

type parameter =
  | Donate(unit)
  | Distribute((unit => list(operation)))

let donate = ((_,s): (unit, storage)) : (list(operation), storage) => {
  (([]: list(operation)), s);
};

let distribute = ((p,s): ((unit => list(operation)), storage)) : (list(operation), storage) => {
  if (Tezos.sender == s) {
    (p(),s);
  }
  else {
    (failwith("You're not the oracle for this distribution."):
      (list(operation), storage));
  };
};

let main = ((p,s): (parameter, storage)) : (list(operation), storage) => {
  switch (p) {
    | Donate => donate (((),s))
    | Distribute msg => distribute ((msg,s))
  };
};
