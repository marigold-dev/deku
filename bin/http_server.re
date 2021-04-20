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

/*

 how to mitigate a 2/3 attack:

 to add an remove a validator, it expects a master chain finality
 if 1/3 of the chain is corrupted at same time, then it stales, no money loss
 if a validator stops,

 if someone votes 2 times, it gets slashed
 if 2/3 votes 2 times, then all the validators that voted 2/3 loose their money and
   everyone that did a double spend looses all their money
 if 2/3 of the chain is corrupted
 */

// TODO: does computers have multiple RTC nowadays?
// Can a core send a message and the other receive it in the past?
// TODO: should start signing before being in sync?

Mirage_crypto_rng_unix.initialize();

open Opium;
open Helpers;
open Protocol;
open Node;

let ignore_some_errors =
  fun
  | Error(#Flows.ignore) => Ok()
  | v => v;
let log_errors = f =>
  fun
  | Ok(_) => ()
  | Error(err) => print_endline(f(err));
let handle_received_block_and_signature = request => {
  open Flows;
  open Networking.Block_and_signature_spec;

  let update_state = state => {
    Server.set_state(state);
    state;
  };
  let.await json = Request.to_json_exn(request);
  let req = {
    let.ok req =
      request_of_yojson(json) |> Result.map_error(err => `Parsing(err));
    let.ok () =
      received_block(Server.get_state(), update_state, req.block)
      |> ignore_some_errors;

    let.ok () =
      received_signature(
        Server.get_state(),
        update_state,
        ~hash=req.block.hash,
        ~signature=req.signature,
      )
      |> ignore_some_errors;
    Ok();
  };
  let status =
    switch (req) {
    | Ok () => `OK
    | Error(_err) => `Internal_server_error
    };
  await(Response.make(~status, ()));
};
let handle_received_block_and_signature =
  App.post(
    Networking.Block_and_signature_spec.path,
    handle_received_block_and_signature,
  );
let handle_received_signature = request => {
  open Flows;
  open Networking.Signature_spec;

  let update_state = state' => {
    Server.set_state(state');
    state';
  };
  let.await json = Request.to_json_exn(request);
  let req = {
    let.ok req =
      request_of_yojson(json) |> Result.map_error(err => `Parsing(err));
    let.ok () =
      received_signature(
        Server.get_state(),
        update_state,
        ~hash=req.hash,
        ~signature=req.signature,
      )
      |> ignore_some_errors;
    Ok();
  };
  let status =
    switch (req) {
    | Ok () => `OK
    | Error(_err) => `Internal_server_error
    };
  await(Response.make(~status, ()));
};
let handle_received_signature =
  App.post(Networking.Signature_spec.path, handle_received_signature);
let handle_requested_block_by_height = request => {
  open Flows;
  open Networking.Block_by_height_spec;

  let.await json = Request.to_json_exn(request);
  let response = {
    let.ok req =
      request_of_yojson(json) |> Result.map_error(err => `Parsing(err));
    let.ok block =
      switch (requested_block_by_height(Server.get_state(), req.block_height)) {
      | Ok(block) => Ok(Some(block))
      | Error(`Unknown_block_height) => Ok(None)
      | Error(err) => Error(err)
      };
    Ok({block: block});
  };
  switch (response) {
  | Ok(response) =>
    let response = response_to_yojson(response);
    await(Response.of_json(~status=`OK, response));
  | Error(_err) => await(Response.make(~status=`Internal_server_error, ()))
  };
};
let handle_requested_block_by_height =
  App.post(
    Networking.Block_by_height_spec.path,
    handle_requested_block_by_height,
  );
let handle_is_applied_block_hash = request => {
  open Flows;
  open Networking.Is_signed_block_hash_spec;
  let.await json = Request.to_json_exn(request);
  let response = {
    let.ok req =
      request_of_yojson(json) |> Result.map_error(err => `Parsing(err));
    let of_hex = str => Hex.to_string(`Hex(str));
    let is_signed =
      is_signed_block_hash(Server.get_state(), of_hex(req.hash));
    Ok({is_signed: is_signed});
  };
  switch (response) {
  | Ok(response) =>
    let response = response_to_yojson(response);
    await(Response.of_json(~status=`OK, response));
  | Error(_err) => await(Response.make(~status=`Internal_server_error, ()))
  };
};
let handle_is_applied_block_hash =
  App.post(
    Networking.Is_signed_block_hash_spec.path,
    handle_is_applied_block_hash,
  );
let handle_block_by_hash = request => {
  open Flows;
  open Networking.Block_by_hash_spec;
  let.await json = Request.to_json_exn(request);
  let response = {
    let.ok req =
      request_of_yojson(json) |> Result.map_error(err => `Parsing(err));
    let block = find_block_by_hash(Server.get_state(), req.hash);
    Ok({block: block});
  };
  switch (response) {
  | Ok(response) =>
    let response = response_to_yojson(response);
    await(Response.of_json(~status=`OK, response));
  | Error(_err) => await(Response.make(~status=`Internal_server_error, ()))
  };
};
let handle_block_by_hash =
  App.post(Networking.Block_by_hash_spec.path, handle_block_by_hash);

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
        State.identity_of_yojson(json);
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

let node = {
  open Utils;
  let identity_file =
    Array.length(Sys.argv) >= 2 ? Sys.argv[1] : "identity.json";
  let.await identity = read_identity_file(identity_file);
  let identity = Result.get_ok(identity);

  let.await validators = read_validators("validators.json");
  let validators = Result.get_ok(validators);

  let node = State.make(~identity);
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
let server = Node.Server.start(~initial=node);

let _server =
  App.empty
  |> App.port(Node.Server.get_port() |> Option.get)
  |> handle_received_block_and_signature
  |> handle_received_signature
  |> handle_requested_block_by_height
  |> handle_is_applied_block_hash
  |> handle_block_by_hash
  |> App.start
  |> Lwt_main.run;

let (forever, _) = Lwt.wait();
let () = Lwt_main.run(forever);
