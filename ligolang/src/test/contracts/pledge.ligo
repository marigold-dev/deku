(* Pledge-Distribute — Accept money from a number of contributors and then donate
   to an address designated by an oracle *)

(* A lot of people (myself included) seem to expect an oracle to be more than it is.
   That is, they expect it to be something complicated when it's actually pretty simple.
   An oracle is just an authorized source of information external to the chain, like an
   arbiter or moderator. For example, it's not possible to do an HTTP request to get
   info from a weather site directly using a smart contract. So instead what you
   do is make (or use) an oracle service which uploads the data to the chain so
   that contracts can use it.
*)

type storage is address

type parameter is
| Donate of unit
| Distribute of (unit -> list(operation))

function donate (const _: unit; const s: storage) : list(operation) * storage is ((nil: list(operation)), s);

function distribute (const p: (unit -> list(operation)); const s: storage) : list(operation) * storage is
  begin
    var result : list(operation) * storage := (p(unit),s);
    if Tezos.sender = s
    then skip
    else result := (failwith("You're not the oracle for this distribution."):
                       list(operation) * storage)
  end with result

function main (const p: parameter; const s: storage) : list(operation) * storage is
  case p of
  | Donate -> donate((unit,s))
  | Distribute (msg) -> distribute((msg,s))
  end
