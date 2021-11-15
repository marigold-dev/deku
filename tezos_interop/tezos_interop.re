open Helpers;
open Crypto;
open Tezos;

let rec try_decode_list = (l, string) =>
  switch (l) {
  | [of_string, ...l] =>
    switch (of_string(string)) {
    | Some(v) => Some(v)
    | None => try_decode_list(l, string)
    }
  | [] => None
  };

module Address = {
  [@deriving eq]
  type t =
    | Implicit(Key_hash.t)
    | Originated({
        contract: Contract_hash.t,
        entrypoint: option(string),
      });

  let contract_encoding =
    Data_encoding.(
      def(
        "contract_id",
        ~title="A contract handle",
        ~description=
          "A contract notation as given to an RPC or inside scripts. Can be a base58 implicit contract hash or a base58 originated contract hash.",
      ) @@
      union(
        ~tag_size=`Uint8,
        [
          case(
            Tag(0),
            ~title="Implicit",
            Key_hash.encoding,
            fun
            | Implicit(k) => Some(k)
            | _ => None,
            k =>
            Implicit(k)
          ),
          case(
            Tag(1),
            Fixed.add_padding(Contract_hash.encoding, 1),
            ~title="Originated",
            fun
            | Originated({contract, _}) => Some(contract)
            | _ => None,
            contract =>
            Originated({contract, entrypoint: None})
          ),
        ],
      )
    );

  let to_string =
    fun
    | Implicit(key_hash) => Key_hash.to_string(key_hash)
    | Originated({contract, entrypoint: None}) =>
      Contract_hash.to_string(contract)
    | Originated({contract, entrypoint: Some(entrypoint)}) =>
      Contract_hash.to_string(contract) ++ "%" ++ entrypoint;
  let of_string = {
    let implicit = string => {
      let.some implicit = Key_hash.of_string(string);
      Some(Implicit(implicit));
    };
    let originated = string => {
      let.some (contract, entrypoint) =
        switch (String.split_on_char('%', string)) {
        | [contract] => Some((contract, None))
        | [contract, entrypoint]
            when String.length(entrypoint) < 32 && entrypoint != "default" =>
          Some((contract, Some(entrypoint)))
        | _ => None
        };
      let.some contract = Contract_hash.of_string(contract);
      Some(Originated({contract, entrypoint}));
    };
    try_decode_list([implicit, originated]);
  };
  let encoding =
    Data_encoding.(
      conv(
        t =>
          switch (t) {
          | Implicit(_) as t => (t, "")
          | Originated({contract: _, entrypoint}) as t =>
            let entrypoint = Option.value(~default="", entrypoint);
            (t, entrypoint);
          },
        fun
        // TODO: should we just discard entrypoint of implicit?
        | (Implicit(_) as t, _) => t
        | (Originated({contract, _}), "" | "default") =>
          Originated({contract, entrypoint: None})
        | (Originated({contract, _}), entrypoint) =>
          Originated({contract, entrypoint: Some(entrypoint)}),
        tup2(contract_encoding, Variable.string),
      )
    );

  let (to_yojson, of_yojson) =
    Yojson_ext.with_yojson_string("address", to_string, of_string);
};

module Ticket = {
  open Tezos_micheline;

  [@deriving eq]
  type t = {
    // TODO: should we allow implicit contracts here?
    ticketer: Address.t,
    data: bytes,
  };

  let parse_micheline = string => {
    let (tokens, errors) = Micheline_parser.tokenize(string);
    switch (errors) {
    | [] =>
      let (micheline, errors) = Micheline_parser.parse_expression(tokens);
      switch (errors) {
      | [] => Some(micheline)
      | _ => None
      };
    | _ => None
    };
  };

  let to_string = t => {
    let loc = Micheline_printer.{comment: None};
    let micheline =
      Micheline.Prim(
        loc,
        "Pair",
        [String(loc, Address.to_string(t.ticketer)), Bytes(loc, t.data)],
        [],
      );
    Format.asprintf("%a", Micheline_printer.print_expr, micheline);
  };
  let of_string = string => {
    let.some micheline = parse_micheline(string);
    let.some (ticketer, data) =
      switch (micheline) {
      // TODO: maybe full Michelson_v1_parser
      | Prim(_, "Pair", [String(_, ticketer), Bytes(_, data)], []) =>
        Some((ticketer, data))
      | _ => None
      };
    let.some ticketer = Address.of_string(ticketer);
    Some({ticketer, data});
  };
};

module Operation_hash = {
  [@deriving eq]
  type t = BLAKE2B.t;

  let prefix = Base58.Prefix.operation_hash;
  let to_raw = BLAKE2B.to_raw_string;
  let of_raw = BLAKE2B.of_raw_string;
  let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
  let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
};

module Pack = {
  open Tezos_micheline;
  open Micheline;
  open Michelson_v1_primitives;

  type t = node(canonical_location, prim);

  let int = n => Int(-1, n);
  let nat = n => Int(-1, n);
  let bytes = b => Bytes(-1, b);
  let pair = (l, r) => Prim(-1, D_Pair, [l, r], []);
  let list = l => Seq(-1, l);
  let key = k =>
    Bytes(-1, Data_encoding.Binary.to_bytes_exn(Key.encoding, k));
  let key_hash = h =>
    Bytes(-1, Data_encoding.Binary.to_bytes_exn(Key_hash.encoding, h));
  let address = addr =>
    Bytes(-1, Data_encoding.Binary.to_bytes_exn(Address.encoding, addr));
  let expr_encoding =
    Micheline.canonical_encoding_v1(
      ~variant="michelson_v1",
      Michelson_v1_primitives.prim_encoding,
    );
  let to_bytes = data =>
    Data_encoding.Binary.to_bytes_exn(expr_encoding, strip_locations(data))
    |> Bytes.cat(Bytes.of_string("\005"));
};

module Context = {
  type t = {
    rpc_node: Uri.t,
    secret: Secret.t,
    consensus_contract: Address.t,
    required_confirmations: int,
  };
};
module Run_contract = {
  [@deriving to_yojson]
  type input = {
    rpc_node: string,
    secret: string,
    confirmation: int,
    destination: string,
    entrypoint: string,
    payload: Yojson.Safe.t,
  };
  type output =
    | Applied({hash: string})
    | Failed({hash: string})
    | Skipped({hash: string})
    | Backtracked({hash: string})
    | Unknown({hash: string})
    | Error(string);

  let output_of_yojson = json => {
    module T = {
      [@deriving of_yojson({strict: false})]
      type t = {status: string}
      and finished = {hash: string}
      and error = {error: string};
    };
    let finished = make => {
      let.ok {hash} = T.finished_of_yojson(json);
      Ok(make(hash));
    };
    let.ok {status} = T.of_yojson(json);
    switch (status) {
    | "applied" => finished(hash => Applied({hash: hash}))
    | "failed" => finished(hash => Failed({hash: hash}))
    | "skipped" => finished(hash => Skipped({hash: hash}))
    | "backtracked" => finished(hash => Backtracked({hash: hash}))
    | "unknown" => finished(hash => Unknown({hash: hash}))
    | "error" =>
      let.ok {error} = T.error_of_yojson(json);
      Ok(Error(error));
    | _ => Error("invalid status")
    };
  };

  // TODO: this leaks the file as it needs to be removed when the app closes
  let file = {
    let.await (file, oc) = Lwt_io.open_temp_file(~suffix=".js", ());
    let.await () = Lwt_io.write(oc, [%blob "run_entrypoint.bundle.js"]);
    await(file);
  };
  let file = Lwt_main.run(file);
  let run = (~context, ~destination, ~entrypoint, ~payload) => {
    let input = {
      rpc_node: context.Context.rpc_node |> Uri.to_string,
      secret: context.secret |> Secret.to_string,
      confirmation: context.required_confirmations,
      destination: Address.to_string(destination),
      entrypoint,
      payload,
    };
    // TODO: stop hard coding this
    let command = "node";
    let.await output =
      Lwt_process.pmap(
        (command, [|command, file|]),
        Yojson.Safe.to_string(input_to_yojson(input)),
      );
    switch (Yojson.Safe.from_string(output) |> output_of_yojson) {
    | Ok(data) => await(data)
    | Error(error) => await(Error(error))
    };
  };
};

let michelson_of_yojson = json => {
  // TODO: do this without serializing
  let.ok json = Yojson.Safe.to_string(json) |> Data_encoding.Json.from_string;
  try(
    Ok(
      Tezos_micheline.Micheline.root(
        Data_encoding.Json.destruct(Pack.expr_encoding, json),
      ),
    )
  ) {
  | _ => Error("invalid json")
  };
};

type michelson =
  Tezos_micheline.Micheline.node(int, Michelson_v1_primitives.prim);
module Fetch_storage: {
  let run:
    (~rpc_node: Uri.t, ~confirmation: int, ~contract_address: Address.t) =>
    Lwt.t(result(michelson, string));
} = {
  [@deriving to_yojson]
  type input = {
    rpc_node: string,
    confirmation: int,
    contract_address: string,
  };
  let output_of_yojson = json => {
    module T = {
      [@deriving of_yojson({strict: false})]
      type t = {status: string}
      and finished = {storage: michelson}
      and error = {error: string};
    };
    let.ok {status} = T.of_yojson(json);
    switch (status) {
    | "success" =>
      let.ok {storage} = T.finished_of_yojson(json);
      Ok(storage);
    | "error" =>
      let.ok T.{error: errorMessage} = T.error_of_yojson(json);
      Error(errorMessage);
    | _ =>
      Error(
        "JSON output %s did not contain 'success' or 'error' for field `status`",
      )
    };
  };

  // TODO: stop hard coding this
  let command = "node";
  let file = {
    let.await (file, oc) = Lwt_io.open_temp_file(~suffix=".js", ());
    let.await () = Lwt_io.write(oc, [%blob "fetch_storage.bundle.js"]);
    await(file);
  };
  let file = Lwt_main.run(file);

  let run = (~rpc_node, ~confirmation, ~contract_address) => {
    let input = {
      rpc_node: Uri.to_string(rpc_node),
      confirmation,
      contract_address: Address.to_string(contract_address),
    };
    let.await output =
      Lwt_process.pmap(
        (command, [|command, file|]),
        Yojson.Safe.to_string(input_to_yojson(input)),
      );
    switch (Yojson.Safe.from_string(output) |> output_of_yojson) {
    | Ok(storage) => await(Ok(storage))
    | Error(error) => await(Error(error))
    };
  };
};

module Listen_transactions = {
  [@deriving of_yojson]
  type output = {
    hash: string,
    index: int,
    entrypoint: string,
    value: michelson,
  };
  module CLI = {
    [@deriving to_yojson]
    type input = {
      rpc_node: string,
      confirmation: int,
      destination: string,
    };
    let file = {
      let.await (file, oc) = Lwt_io.open_temp_file(~suffix=".js", ());
      let.await () =
        Lwt_io.write(oc, [%blob "listen_transactions.bundle.js"]);
      await(file);
    };
    let file = Lwt_main.run(file);

    let node = "node";
    let run = (~context, ~destination, ~on_message, ~on_fail) => {
      let send = (f, pr, data) => {
        let oc = pr#stdin;
        Lwt.finalize(() => f(oc, data), () => Lwt_io.close(oc));
      };

      let process = Lwt_process.open_process((node, [|node, file|]));
      let input =
        {
          rpc_node: Uri.to_string(context.Context.rpc_node),
          confirmation: context.required_confirmations,
          destination: Address.to_string(destination),
        }
        |> input_to_yojson
        |> Yojson.Safe.to_string;
      let on_fail = _exn => {
        // TODO: what to do with this exception
        // TODO: what to do with this status
        let.await _status = process#close;
        on_fail();
      };
      let.await () = send(Lwt_io.write, process, input);

      let rec read_line_until_fails = () =>
        Lwt.catch(
          () => {
            let.await line = Lwt_io.read_line(process#stdout);
            print_endline(line);
            Yojson.Safe.from_string(line)
            |> output_of_yojson
            |> Result.get_ok
            |> on_message;
            read_line_until_fails();
          },
          on_fail,
        );
      read_line_until_fails();
    };
  };

  let listen = (~context, ~destination, ~on_message) => {
    let rec start = () =>
      Lwt.catch(
        () => CLI.run(~context, ~destination, ~on_message, ~on_fail),
        // TODO: what to do with this exception?
        _exn => on_fail(),
      )
    and on_fail = () => start();
    Lwt.async(start);
  };
};
module Consensus = {
  open Pack;
  open Tezos_micheline;

  let hash_packed_data = data =>
    data |> to_bytes |> Bytes.to_string |> BLAKE2B.hash;

  let hash_validators = validators =>
    list(List.map(key_hash, validators)) |> hash_packed_data;
  let hash = hash => bytes(BLAKE2B.to_raw_string(hash) |> Bytes.of_string);
  let hash_block =
      (
        ~block_height,
        ~block_payload_hash,
        ~state_root_hash,
        ~handles_hash,
        ~validators_hash,
      ) =>
    pair(
      pair(
        pair(int(Z.of_int64(block_height)), hash(block_payload_hash)),
        pair(hash(handles_hash), hash(state_root_hash)),
      ),
      hash(validators_hash),
    )
    |> hash_packed_data;
  let hash_withdraw_handle = (~id, ~owner, ~amount, ~ticketer, ~data) =>
    pair(
      pair(
        pair(nat(amount), bytes(data)),
        pair(nat(id), address(owner)),
      ),
      address(ticketer),
    )
    |> hash_packed_data;

  // TODO: how to test this?
  let commit_state_hash =
      (
        ~context,
        ~block_hash,
        ~block_height,
        ~block_payload_hash,
        ~state_hash,
        ~handles_hash,
        ~validators,
        ~signatures,
      ) => {
    module Payload = {
      [@deriving to_yojson]
      type t = {
        block_hash: BLAKE2B.t,
        block_height: int64,
        block_payload_hash: BLAKE2B.t,
        signatures: list(option(string)),
        handles_hash: BLAKE2B.t,
        state_hash: BLAKE2B.t,
        validators: list(string),
      };
    };
    open Payload;
    let signatures =
      // TODO: we should sort the map using the keys
      List.map(
        ((_key, signature)) =>
          Option.map(signature => Signature.to_string(signature), signature),
        signatures,
      );
    let validators = List.map(Key_hash.to_string, validators);
    let payload = {
      block_hash,
      block_height,
      block_payload_hash,
      signatures,
      handles_hash,
      state_hash,
      validators,
    };
    // TODO: what should this code do with the output? Retry?
    //      return back that it was a failure?
    let.await _ =
      Run_contract.run(
        ~context,
        ~destination=context.Context.consensus_contract,
        ~entrypoint="update_root_hash",
        ~payload=Payload.to_yojson(payload),
      );
    await();
  };

  type parameters =
    | Deposit({
        ticket: Ticket.t,
        // TODO: proper type for amounts
        amount: Z.t,
        destination: Address.t,
      })
    | Update_root_hash(BLAKE2B.t);
  type operation = {
    hash: BLAKE2B.t,
    index: int,
    parameters,
  };

  let parse_parameters = (entrypoint, micheline) =>
    switch (entrypoint, micheline) {
    | (
        "update_root_hash",
        Tezos_micheline.Micheline.Prim(
          _,
          Michelson_v1_primitives.D_Pair,
          [
            Prim(
              _,
              D_Pair,
              [
                Prim(
                  _,
                  D_Pair,
                  [Bytes(_, _block_hash), Int(_, _block_height)],
                  _,
                ),
                Prim(
                  _,
                  D_Pair,
                  [Bytes(_, _block_payload_hash), Int(_, _handles_hash)],
                  _,
                ),
              ],
              _,
            ),
            Prim(
              _,
              D_Pair,
              [
                Prim(_, D_Pair, [_signatures, Bytes(_, state_root_hash)], _),
                _,
              ],
              _,
            ),
          ],
          _,
        ),
      ) =>
      let.some state_root_hash =
        state_root_hash |> Bytes.to_string |> BLAKE2B.of_raw_string;
      Some(Update_root_hash(state_root_hash));
    | (
        "deposit",
        Micheline.Prim(
          _,
          Michelson_v1_primitives.D_Pair,
          [
            Bytes(_, destination),
            Prim(
              _,
              D_Pair,
              [
                Bytes(_, ticketer),
                Prim(_, D_Pair, [Bytes(_, data), Int(_, amount)], _),
              ],
              _,
            ),
          ],
          _,
        ),
      ) =>
      let.some destination =
        Data_encoding.Binary.of_bytes_opt(Address.encoding, destination);
      let.some ticketer =
        Data_encoding.Binary.of_bytes_opt(Address.encoding, ticketer);
      let ticket = Ticket.{ticketer, data};
      Some(Deposit({ticket, destination, amount}));
    | _ => None
    };
  let parse_operation = output => {
    let.some parameters =
      parse_parameters(output.Listen_transactions.entrypoint, output.value);
    let.some hash = Operation_hash.of_string(output.hash);
    Some({hash, index: output.index, parameters});
  };
  let listen_operations = (~context, ~on_operation) => {
    let on_message = output =>
      switch (parse_operation(output)) {
      | Some(operation) => on_operation(operation)
      | None => ()
      };
    Listen_transactions.listen(
      ~context,
      ~destination=context.consensus_contract,
      ~on_message,
    );
  };
  let fetch_validators = (~context) => {
    let Context.{rpc_node, required_confirmations, consensus_contract, _} = context;
    let micheline_to_validators =
      fun
      | Ok(
          Micheline.Prim(
            _,
            Michelson_v1_primitives.D_Pair,
            [Prim(_, D_Pair, [_, Seq(_, key_hashes)], _), _, _],
            _,
          ),
        ) => {
          List.fold_left_ok(
            (acc, k) =>
              switch (k) {
              | Micheline.String(_, k) =>
                switch (Key_hash.of_string(k)) {
                | Some(k) => Ok([k, ...acc])
                | None => Error("Failed to parse " ++ k)
                }
              | _ => Error("Some key_hash wasn't of type string")
              },
            [],
            List.rev(key_hashes),
          );
        }
      | Ok(_) => Error("Failed to parse storage micheline expression")
      | Error(msg) => Error(msg);
    let.await micheline_storage =
      Fetch_storage.run(
        ~confirmation=required_confirmations,
        ~rpc_node,
        ~contract_address=consensus_contract,
      );
    Lwt.return(micheline_to_validators(micheline_storage));
  };
};

module Discovery = {
  open Pack;

  let sign = (secret, ~nonce, uri) =>
    to_bytes(
      pair(
        int(Z.of_int64(nonce)),
        bytes(Bytes.of_string(Uri.to_string(uri))),
      ),
    )
    |> Bytes.to_string
    |> BLAKE2B.hash
    |> Signature.sign(secret);
};
