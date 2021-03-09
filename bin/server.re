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

/*
 a block is only considered received AFTER it is json parsed,
 after the JSON is parsed all the requests are sync,
 otherwise it needs to lock the state
 */

open Opium;
open Helpers;
open Protocol;
open Node;

module Utils = {
  let read_file = file => {
    let.await ic = Lwt_io.open_file(~mode=Input, file);
    let.await lines = Lwt_io.read_lines(ic) |> Lwt_stream.to_list;
    let.await () = Lwt_io.close(ic);
    await(lines |> String.concat("\n"));
  };

  let read_identity_file = file => {
    let.await file_buffer = read_file(file);
    await(
      try({
        let json = Yojson.Safe.from_string(file_buffer);
        Node.identity_of_yojson(json);
      }) {
      | _ => Error("failed to parse json")
      },
    );
  };
  // TODO: write only file system signed by identity key and in binary identity key
  let read_validators = file => {
    let.await file_buffer = read_file(file);
    await(
      try({
        let json = Yojson.Safe.from_string(file_buffer);
        [%of_yojson: list(Validators.validator)](json);
      }) {
      | _ => Error("failed to parse json")
      },
    );
  };
};

// TODO: load snapshot
type server = {
  // TODO: lock state
  mutable state: t,
  mutable timeout: Lwt.t(unit),
};

let node = {
  open Utils;
  let.await identity = read_identity_file("identity.json");
  let identity = Result.get_ok(identity);

  let.await validators = read_validators("validators.json");
  let validators = Result.get_ok(validators);

  let node = Node.make(~identity);
  let node = {
    ...node,
    protocol: {
      ...node.protocol,
      validators:
        List.fold_right(Validators.add, validators, node.protocol.validators),
    },
  };
  await(node);
};
let node = node |> Lwt_main.run;
let server = {state: node, timeout: Lwt.return_unit};

let timeout_happened = () => {
  let _x = assert(false);
  ();
};
let reset_block_timeout = () => {
  Lwt.cancel(server.timeout);
  server.timeout = Lwt_unix.sleep(10.0) |> Lwt.map(timeout_happened);
};

let produce_block = state => {
  let (signatures, block) = make_block(state);
  Lwt.async(() => {
    let.await () = Lwt_unix.sleep(1.0);
    broadcast(state, (module Signed_block), {signatures, block});
  });
};

let dispatch_side_effect = (state, side_effect) => {
  switch (side_effect) {
  | Main_operation => ()
  | Side_operation({signature, operation}) =>
    Lwt.async(() =>
      broadcast(state, (module Side_chain_operation), {operation, signature})
    )
  | Valid_block({signatures, block}) =>
    reset_block_timeout();
    Lwt.async(() =>
      broadcast(state, (module Signed_block), {signatures, block})
    );
    if (is_time_to_make_block(state)) {
      produce_block(state);
    };
  };
};

exception Invalid_request_json;
let post_request =
  [
    ((module Side_chain_operation): (module Side_effect_endpoint)),
    ((module Main_chain_operation): (module Side_effect_endpoint)),
    ((module Signed_block): (module Side_effect_endpoint)),
  ]
  |> List.map((module E: Side_effect_endpoint) => {
       App.post(
         E.path,
         request => {
           let.await json = Request.to_json_exn(request);
           let req = {
             let.ok req = E.request_of_yojson(json);
             let.ok {state: new_state, side_effect} =
               E.handle_request(server.state, req);
             switch (side_effect) {
             | Some(side_effect) =>
               dispatch_side_effect(new_state, side_effect)
             | _ => ()
             };
             server.state = new_state;
             Ok();
           };
           let status =
             switch (req) {
             | Ok () => `OK
             | Error(err) =>
               print_endline(err);
               `Internal_server_error;
             };
           await(Response.make(~status, ()));
         },
       )
     });
let () =
  App.empty
  |> App.port(server.state.identity.uri |> Uri.port |> Option.get)
  |> App.get("/", _request =>
       Response.of_json(`String("lol")) |> Lwt.return
     )
  |> List.fold_left((|>), _, post_request)
  |> App.run_command;
