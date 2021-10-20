open Setup;
open Protocol;
open Address;
open Tezos_interop;
open Tezos;
let some_contract_hash =
  Contract_hash.of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc")
  |> Option.get;
// TODO: maybe fuzz testing or any other cool testing magic?
describe("pack", ({test, _}) => {
  open Pack;

  // TODO: use ligo to prevent regressions

  let test = (name, input, output) =>
    test(
      name,
      ({expect, _}) => {
        let `Hex(result) = to_bytes(input) |> Hex.of_bytes;
        expect.string(result).toEqual(output);
      },
    );

  let int = n => int(Z.of_int(n));
  let bytes = s => bytes(Bytes.of_string(Hex.to_string(`Hex(s))));

  test("int(1)", int(1), "050001");
  test("int(-1)", int(-1), "050041");
  test("bytes(0x)", bytes(""), "050a00000000");
  test("bytes(0x050001)", bytes("050001"), "050a00000003050001");
  test("pair(1, 0x)", pair(int(1), bytes("")), "05070700010a00000000");
  test(
    "pair(1, (0xAA, -1))",
    pair(int(1), pair(bytes("AA"), int(-1))),
    "050707000107070a00000001aa0041",
  );
  test("list([])", list([]), "050200000000");
  test("list([1])", list([int(1)]), "0502000000020001");
  test(
    "list([(1, (0x, -1))])",
    list([pair(int(1), pair(bytes(""), int(-1)))]),
    "05020000000d0707000107070a000000000041",
  );
  test(
    "key(\"edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6\")",
    key(genesis_address),
    "050a0000002100d00725159de904a28aaed9adb2320f95bd2117959e41c1c2377ac11045d18bd7",
  );
  test(
    "key_hash(\"tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC\")",
    key_hash(Key_hash.of_key(genesis_address)),
    "050a00000015000ec89608700c0414159d93552ef9361cea96da13",
  );
  test(
    "address(\"tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC\")",
    address(Implicit(Key_hash.of_key(genesis_address))),
    "050a0000001600000ec89608700c0414159d93552ef9361cea96da13",
  );
  test(
    "address(\"KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc\")",
    address(Originated({contract: some_contract_hash, entrypoint: None})),
    "050a0000001601370027c6c8f3fbafda4f9bfd08b14f45e6a29ce300",
  );
});
describe("consensus", ({test, _}) => {
  open Helpers;
  open Consensus;

  let hash_exn = s => BLAKE2B.of_string(s) |> Option.get;
  let key_exn = s => Key.of_string(s) |> Option.get;
  let address_exn = s => Address.of_string(s) |> Option.get;

  test("hash_validators", ({expect, _}) => {
    let hash =
      hash_validators([
        key_exn("edpkvQuAn9BeaDQLzudrPL2zigNRQSmFvKJ7xWN1QmjDjQHj3dBrEZ"),
        key_exn("edpkvE3Ysq17HFzBBSQeAX87RE3smYZf1rHHpKu1LJdaFAhW8G7SNu"),
        key_exn("edpktq5HiqUkHTyoBQETvzbyaiwtKQkaBEPkwgZyfMqhRajRuLpWR7"),
        key_exn("edpkuNpThN8QeagEdvjN3o5R7PSic85cwiXHa61vNpRAE65FNV5mJH"),
      ]);
    let hash = BLAKE2B.to_string(hash);
    expect.string(hash).toEqual(
      "546d2bb2375cc919efc81a103a7ad3bd1227546b320f275e357bd9a5d5eef946",
    );
  });
  test("hash_block", ({expect, _}) => {
    let hash =
      hash_block(
        ~block_height=121L,
        ~block_payload_hash=
          hash_exn(
            "2d92960a592c56de3046e200969c230a2eda71fc4b775e0cc09a189e5ddc5dbd",
          ),
        ~state_root_hash=
          hash_exn(
            "bdd051ddb07925a0d88dc27583e38ae560aa1b4429cc93b9ec35dacdbd74ffb2",
          ),
        ~handles_hash=
          hash_exn(
            "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
          ),
        ~validators_hash=
          hash_exn(
            "546d2bb2375cc919efc81a103a7ad3bd1227546b320f275e357bd9a5d5eef946",
          ),
      );
    let hash = BLAKE2B.to_string(hash);
    expect.string(hash).toEqual(
      "7cb600c19817b899d4c28c521dd9ebf95f688e1444afe7d0e7740bebe848b030",
    );
  });
  test("hash_withdraw_handle", ({expect, _}) => {
    let hash =
      hash_withdraw_handle(
        ~id=Z.of_int(0),
        ~owner=address_exn("tz1YywYq77UAMbVgoYndnZLkRawjUhX3nVh4"),
        ~amount=Z.of_int(10),
        ~ticketer=address_exn("KT1AS9rCk1wpybsvZ5Tnd4yRxDvtN39uxMoq"),
        ~data=Bytes.of_string(""),
      );
    let hash = BLAKE2B.to_string(hash);
    expect.string(hash).toEqual(
      "63dfd90ec7be98a9c23bf374692de4d36f41fe03c4c768fc0c650641d3ed4f86",
    );
  });
});
describe("discovery", ({test, _}) => {
  open Discovery;

  let secret = genesis_key;
  test("sign", ({expect, _}) => {
    let signature =
      sign(secret, ~nonce=1L, Uri.of_string("http://localhost"));
    expect.string(Signature.to_string(signature)).toEqual(
      "edsigtpGEA7XKPKMFkFiEA6SfJaaha4Ai2XbteJG5FYvuMtMRyPnXRuZNi54P7BWvV6GaWTijf8EBjGb8MZZvdTrWCGFCVCXL7r",
    );
  });
});
