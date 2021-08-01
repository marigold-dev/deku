open Helpers;
open Mirage_crypto_ec;

let (let.some) = Option.bind;

let rec try_decode_list = (l, string) =>
  switch (l) {
  | [of_string, ...l] =>
    switch (of_string(string)) {
    | Some(v) => Some(v)
    | None => try_decode_list(l, string)
    }
  | [] => None
  };

let blake2b_20_encoding =
  Data_encoding.(
    conv(
      hash => BLAKE2B_20.to_raw_string(hash) |> Bytes.of_string,
      // TODO: I don't like this exception below
      bytes =>
        Bytes.to_string(bytes) |> BLAKE2B_20.of_raw_string |> Option.get,
      Fixed.bytes(20),
    )
  );
module Base58 = Base58;

module Ed25519 = {
  open Ed25519;

  type t = pub_;

  let size = 32;
  let prefix = Base58.Prefix.ed25519_public_key;
  let encoding = {
    // TODO: in tezos this is splitted json is not same as binary
    let to_bytes = t => pub_to_cstruct(t) |> Cstruct.to_bytes;
    let of_bytes_exn = b =>
      Cstruct.of_bytes(b) |> pub_of_cstruct |> Result.get_ok;
    Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(size)));
  };

  let to_raw = t => Cstruct.to_string(Ed25519.pub_to_cstruct(t));
  let of_raw = string =>
    Ed25519.pub_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
  let to_string = Base58.simple_encode(~prefix, ~to_raw);
  let of_string = Base58.simple_decode(~prefix, ~of_raw);

  module Hash = {
    type t = BLAKE2B_20.t;

    let hash_key = t =>
      BLAKE2B_20.hash(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string);

    let encoding = {
      let name = "Ed25519.Public_key_hash";
      // TODO: in tezos this is splitted json is not same as bin
      Data_encoding.(obj1(req(name, blake2b_20_encoding)));
    };

    let prefix = Base58.Prefix.ed25519_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };

  module Secret = {
    type t = priv;

    let _size = 32;
    let prefix = Base58.Prefix.ed25519_seed;
    let to_raw = t => Cstruct.to_string(Ed25519.priv_to_cstruct(t));
    let of_raw = string =>
      Ed25519.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;

    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
  module Signature = {
    type t = string;
    let sign = (secret, message) => {
      // double hash because tezos always uses blake2b on CHECK_SIGNATURE
      let hash = BLAKE2B.hash(message);
      Cstruct.of_string(BLAKE2B.to_raw_string(hash))
      // TODO: isn't this double hashing? Seems weird
      |> Ed25519.sign(~key=secret)
      |> Cstruct.to_string;
    };
    let check = (public, signature, message) => {
      let hash = BLAKE2B.hash(message);
      verify(
        ~key=public,
        ~msg=Cstruct.of_string(BLAKE2B.to_raw_string(hash)),
        Cstruct.of_string(signature),
      );
    };

    let size = 64;
    let prefix = Base58.Prefix.ed25519_signature;
    let to_raw = Fun.id;
    let of_raw = string =>
      String.length(string) == size ? Some(string) : None;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
};

module Key = {
  type t =
    | Ed25519(Ed25519.t);

  let name = "Signature.Public_key";
  let title = "A Ed25519, Secp256k1, or P256 public key";
  let encoding = {
    open Data_encoding;
    let raw_encoding =
      def("public_key", ~description=title) @@
      union([
        case(
          Tag(0),
          Ed25519.encoding,
          ~title="Ed25519",
          fun
          | Ed25519(x) => Some(x),
          x =>
          Ed25519(x)
        ),
      ]);

    // TODO: move this to a functor
    obj1(req(name, raw_encoding));
  };
  let to_string =
    fun
    | Ed25519(key) => Ed25519.to_string(key);
  let of_string = {
    let ed25519 = string => {
      let.some key = Ed25519.of_string(string);
      Some(Ed25519(key));
    };
    try_decode_list([ed25519]);
  };
};

module Key_hash = {
  type t =
    | Ed25519(Ed25519.Hash.t);

  let name = "Signature.Public_key_hash";

  let title = "A Ed25519, Secp256k1, or P256 public key hash";
  let encoding = {
    open Data_encoding;
    let raw_encoding =
      def("public_key_hash", ~description=title) @@
      union([
        case(
          Tag(0),
          Ed25519.Hash.encoding,
          ~title="Ed25519",
          fun
          | Ed25519(x) => Some(x),
          x =>
          Ed25519(x)
        ),
      ]);
    obj1(req(name, raw_encoding));
  };

  let of_key = t =>
    switch (t) {
    | Key.Ed25519(pub_) => Ed25519(Ed25519.Hash.hash_key(pub_))
    };

  let to_string =
    fun
    | Ed25519(hash) => Ed25519.Hash.to_string(hash);
  let of_string = {
    let ed25519 = string => {
      let.some key = Ed25519.Hash.of_string(string);
      Some(Ed25519(key));
    };
    try_decode_list([ed25519]);
  };
};
module Secret = {
  type t =
    | Ed25519(Ed25519.Secret.t);

  let to_string =
    fun
    | Ed25519(secret) => Ed25519.Secret.to_string(secret);
  let of_string = {
    let ed25519 = string => {
      let.some secret = Ed25519.Secret.of_string(string);
      Some(Ed25519(secret));
    };
    try_decode_list([ed25519]);
  };
};
module Contract_hash = {
  type t = BLAKE2B_20.t;

  let name = "Contract_hash";
  let encoding = Data_encoding.(obj1(req(name, blake2b_20_encoding)));
  let to_raw = BLAKE2B_20.to_raw_string;
  let of_raw = BLAKE2B_20.of_raw_string;
  let prefix = Base58.Prefix.contract_hash;
  let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
  let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
};
module Address = {
  // TODO: there is also contract_hash with entrypoint
  type t =
    | Implicit(Key_hash.t)
    | Originated(Contract_hash.t);

  let encoding =
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
            | Originated(k) => Some(k)
            | _ => None,
            k =>
            Originated(k)
          ),
        ],
      )
    );

  let to_string =
    fun
    | Implicit(key_hash) => Key_hash.to_string(key_hash)
    | Originated(contract_hash) => Contract_hash.to_string(contract_hash);
  let of_string = {
    let implicit = string => {
      let.some implicit = Key_hash.of_string(string);
      Some(Implicit(implicit));
    };
    let originated = string => {
      let.some originated = Contract_hash.of_string(string);
      Some(Originated(originated));
    };
    try_decode_list([implicit, originated]);
  };

  let with_yojson_string = (name, of_string, to_string) =>
    Helpers.with_yojson_string(
      string =>
        of_string(string) |> Option.to_result(~none="invalid " ++ name),
      to_string,
    );
  let (of_yojson, to_yojson) =
    with_yojson_string("address", of_string, to_string);
};

module Signature = {
  type t =
    | Ed25519(Ed25519.Signature.t);

  let sign = (secret, message) =>
    switch (secret) {
    | Secret.Ed25519(secret) =>
      Ed25519(Ed25519.Signature.sign(secret, message))
    };
  let check = (key, signature, message) =>
    switch (key, signature) {
    | (Key.Ed25519(key), Ed25519(signature)) =>
      Ed25519.Signature.check(key, signature, message)
    };
  let to_string =
    fun
    | Ed25519(sign) => Ed25519.Signature.to_string(sign);
  let of_string = {
    let ed25519 = string => {
      let.some sign = Ed25519.Signature.of_string(string);
      Some(Ed25519(sign));
    };
    try_decode_list([ed25519]);
  };
  // TODO: this is a leaky abstraction
  let of_raw_string =
    fun
    | `Ed25519(data) => Ed25519(data);
};

module Ticket = {
  open Tezos_micheline;

  type t = {
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
    Bytes(
      -1,
      Data_encoding.Binary.to_bytes_exn(
        Data_encoding.(tup2(Address.encoding, Variable.string)),
        (addr, ""),
      ),
    );
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

module Consensus = {
  open Pack;

  let hash_packed_data = data =>
    data |> to_bytes |> Bytes.to_string |> BLAKE2B.hash;

  let hash_validators = validators =>
    list(List.map(key, validators)) |> hash_packed_data;
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
    let validators = List.map(Key.to_string, validators);
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
};

module Discovery = {
  open Pack;

  let sign = (secret, ~nonce, uri) =>
    Signature.sign(
      secret,
      Bytes.to_string(
        to_bytes(
          pair(
            int(Z.of_int64(nonce)),
            bytes(Bytes.of_string(Uri.to_string(uri))),
          ),
        ),
      ),
    );
};
