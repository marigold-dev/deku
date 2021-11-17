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

type storage = address

type parameter =
| Donate of unit
| Distribute of (unit -> operation list)

let donate ((_,s): unit * storage) : operation list * storage = ([]: operation list), s

let distribute ((p,s): (unit -> operation list) * storage) : operation list * storage =
  if Tezos.sender = s
  then p (),s
  else (failwith "You're not the oracle for this distribution.":
          operation list * storage)

let main ((p,s): parameter * storage) : operation list * storage =
  match p with
  | Donate -> donate ((),s)
  | Distribute msg -> distribute (msg,s)
