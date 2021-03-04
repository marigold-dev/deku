/*

 - block
 - wallet
 - transactions
 - swaps (cross-chain atomic swaps)

 ## State

 - balance
 - frozen blances
 - validator list
 - current block producer + timeout + round-robin

 ## Consensus

 reasonable 50ms-100ms

 - proof-of-authority

 A : Alice
 B : Bob

 X : Continent Europe
 Y : Continent America

 message by X, signed by 2/3 of all nodes in X
 so if 2/3 of X agrees on the block, X agreed on the block

 X -> Y -> Z -> X
 X¹ -> X² -> X³ -> X¹

 X block for X -> X transanctions

 A(X) -> B(X), consensus of X¹ and X²
 A(X) -> B(Y), consensus of X and Y
 A(X) -> C(Z), consensus of X and Y

 G is global current-block-producer
 G commits A(X) -> B(Y)

 2 choices:
 - G commits on local blocks + global transactions
 - **G unfolds local blocks into transactions, + G commits on all transactions**

 every 10 blocks, local commit to global.

 2 models:
 i. some state is purely local = G can not change it. so when you are on a local metanode, you have two balances: local balance and global balance. moving one to the other takes time.
 ii. G can change everything. so regularly, data is locked, at which points no tx can happen.

 i
   never locked, so always quick. bad UX: user must know about local vs global.
   worst-case local = 1 local round trip
   worst-case global = 1 global round trip (1 local -> global + global -> global)

 ii
   sometimes locked, and intrinsic tradeoffs: either stuff is locked regularly,
   and local tx are slower. or stuff is locked rarely, and global tx are slower.
   good UX: user don't care about local vs global.
   worst-case local = 1 local round-trip if not locked / 1 global round-trip if locked. ~100ms / ~1s
   worst-case global = waiting for the lock + 1 global operation. ~10s

 ii contracts -> i contracts
 i+ii?

 iii
   a global can happen at anytime, but if it touches local data,
   it must lock, lock takes 2 blocks, op + unlock takes 2 blocks.
   worst-case local = 2 global round-trip + 2 blocks

 **conclusion: start with i, later move to i+ii, lazyness yay**

 95% = local
 5% = global

 1 block every 10 is global
 1 block = 500 ms
 lock time = 2 block

 5% tx need to wait for 10seconds


 i

 Smart-contracts that touch state that is only in X, or in X and Y

 If smart-contract only touches state in X, X's consensus is enough
 If smart-contract touches state in multiple places, it's global consensus

 local

   A is in X
   B is in X

   1. current block producer aggregates transactions (between people)
   2. at a given time, put them in a block, signs the block, broadcast it
   3. everyone else signs the block, broadcast signatures
   4. go back to step 1
   4 bis. if people don't get a signed block after a timeout, broadcasts message stating so
   5 bis. if everyone gets 2/3rd stating so, move to the next block producer

 global

   A is in X
   B is in Y

   1. current block producer aggregates transactions (between people)


 */

/*



 */

/*

 # HTTP


 ## Users

 POST /transaction
 POST /freeze

 ## Tezos Daemon

 POST /last-updates
 -> sends a list of updates in Tezos + number of the latest one.

 ## Nodes

 POST /broadcast-signed-block
 POST /broadcast-transaction

 */

open Opium;
open Protocol;
open Protocol_state;
open Node;
open Node_state;
open Helpers;

module Signature = {
  open Mirage_crypto;
  open Mirage_crypto_pk;
  module Rsa_sha256 = Rsa.PSS(Hash.SHA256);
  let sign = (~key, data) => {
    let data = Marshal.to_string(data, []);
    Rsa_sha256.sign(~key, `Message(Cstruct.of_string(data)))
    |> Cstruct.to_string;
  };
  let verify = (~pub_, ~signature, data) => {
    let data = Marshal.to_string(data, []);
    Rsa_sha256.verify(
      ~key=pub_,
      ~signature=Cstruct.of_string(signature),
      `Message(Cstruct.of_string(data)),
    );
  };
};

let read_file = file => {
  let.await ic = Lwt_io.open_file(~mode=Input, file);
  let.await lines = Lwt_io.read_lines(ic) |> Lwt_stream.to_list;
  let.await () = Lwt_io.close(ic);
  await(lines |> String.concat("\n"));
};
let read_private_key = file => {
  open Mirage_crypto_pk;
  let.await key_buffer = read_file(file);
  key_buffer |> Sexplib.Sexp.of_string |> Rsa.priv_of_sexp |> await;
};
let read_validators = file => {
  let.await validator_buffer = read_file(file);
  String.split_on_char('\n', validator_buffer)
  |> List.map(Uri.of_string)
  |> await;
};

let identity = read_private_key("priv.key") |> Lwt_main.run;
let validators = read_validators("validators.txt") |> Lwt_main.run;

// TODO: load snapshot
let protocol_state = Protocol_state.empty;
let node_state =
  ref(Node_state.make(~identity, ~validators, ~protocol_state));

[@deriving yojson]
type operation_request = {
  signature: string,
  operation: Operation.side_chain_ops,
};

/*

 timeout when a block doesn't send, bootstrapped

 A -> B -> C -> D
 A is the current producer
 A sends a block every 3 seconds.
 0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15
 ^ - - - - - ^ - - - - - - - - - - - A
                                     B1            B2
                                       - -- - -- - C


 0 - A
 0.5s - B
 0.6s - C

 start counting when you receive the last block

 */
let operation =
  App.post("/operation", req => {
    // TODO: remove exceptions here
    let.await json = Request.to_json_exn(req);
    // TODO: exceptions here
    let {signature, operation} =
      operation_request_of_yojson(json) |> Result.get_ok;
    let pub_ = Operation.get_side_chain_op_source(operation);
    if (!Signature.verify(~pub_, ~signature, operation)) {
      raise(Invalid_argument("invalid signature for operation"));
    };

    // TODO: 204
    let response = Response.of_plain_text("ok") |> Lwt.return;
    // TODO: check if it's in the list, to avoid infinite recursion network stuff
    let state = node_state^;
    if (state.pending_side_ops |> Operation.Side_chain_ops_set.mem(operation)) {
      response;
    } else {
      let state = {
        ...state,
        pending_side_ops:
          state.pending_side_ops
          |> Operation.Side_chain_ops_set.add(operation),
      };
      node_state := state;
      // TODO: is it okay to be async?
      // TODO: what if it fails to broadcast?
      Lwt.async(() =>
        Node_client.broadcast_side_ops(state, ~signature, operation)
      );
      response;
    };
  });

let index =
  App.get("/", _request => Response.of_json(`String("lol")) |> Lwt.return);

App.empty |> index |> App.run_command;
