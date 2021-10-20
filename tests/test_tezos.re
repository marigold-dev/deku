open Setup;
open Protocol;
open Address;
open Tezos;
// TODO: maybe fuzz testing or any other cool testing magic?
let some_contract_hash =
  Contract_hash.of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc")
  |> Option.get;
describe("key", ({test, _}) => {
  open Key;

  // TODO: test encoding

  let edpk = genesis_address;
  test("to_string", ({expect, _}) => {
    expect.string(to_string(edpk)).toEqual(
      "edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6",
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(
      of_string("edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6"),
    ).
      toBe(
      ~equals=Key.equal,
      Some(edpk),
    )
  });
  test("invalid prefix", ({expect, _}) => {
    // TODO: this test would fail anyway becose of the checksum
    //       craft an example with different prefix and valid checksum
    // TODO: when this test fails it segfaults
    // see https://github.com/reasonml/reason-native/pull/263
    expect.option(
      of_string("edpuvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6"),
    ).
      toBeNone()
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(
      of_string("edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT5"),
    ).
      toBeNone()
  });
  test("invalid size", ({expect, _}) => {
    // TODO: this test would fail anyway becose of the checksum
    //       craft an example with different size but valid checksum
    expect.option(
      of_string("edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT"),
    ).
      toBeNone()
  });
});
describe("key_hash", ({test, _}) => {
  open Key_hash;

  // TODO: proper test of_key
  let tz1 = of_key(genesis_address);
  test("to_string", ({expect, _}) => {
    expect.string(to_string(tz1)).toEqual(
      "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC",
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBe(
      ~equals=Key_hash.equal,
      Some(tz1),
    )
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(of_string("tzaLzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBeNone()
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLA")).toBeNone()
  });
  test("invalid size", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCL")).toBeNone()
  });
});
describe("secret", ({test, _}) => {
  open Secret;

  let edsk = genesis_key;
  test("to_string", ({expect, _}) => {
    expect.string(to_string(edsk)).toEqual(
      "edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd",
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(
      of_string("edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd"),
    ).
      toBe(
      ~equals=Secret.equal,
      Some(edsk),
    )
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(
      of_string("edsa4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd"),
    ).
      toBeNone()
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(
      of_string("edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVb"),
    ).
      toBeNone()
  });
  test("invalid size", ({expect, _}) => {
    expect.option(
      of_string("edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSV"),
    ).
      toBeNone()
  });
});
describe("signature", ({test, _}) => {
  open Signature;

  let edpk = genesis_address;
  let edsk = genesis_key;

  // TODO: proper test for sign
  let edsig = sign(edsk, "tuturu");

  test("check", ({expect, _}) => {
    expect.bool(check(edpk, edsig, "tuturu")).toBeTrue()
  });
  test("invalid message", ({expect, _}) => {
    expect.bool(check(edpk, edsig, "tuturu2")).toBeFalse()
  });
  test("invalid key", ({expect, _}) => {
    let (secret, key) = {
      let (secret, key) = Crypto.Ed25519.generate();
      (Secret.Ed25519(secret), Key.Ed25519(key));
    };
    let edsig_from_key = sign(secret, "tuturu");
    expect.bool(check(key, edsig_from_key, "tuturu")).toBeTrue();
    expect.bool(check(edpk, edsig_from_key, "tuturu")).toBeFalse();
  });

  test("to_string", ({expect, _}) => {
    expect.string(to_string(edsig)).toEqual(
      "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4",
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(
      of_string(
        "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4",
      ),
    ).
      toBe(
      ~equals=Signature.equal,
      Some(edsig),
    )
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(
      of_string(
        "edsiatp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4",
      ),
    ).
      toBeNone()
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(
      of_string(
        "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq3",
      ),
    ).
      toBeNone()
  });
  test("invalid size", ({expect, _}) => {
    expect.option(
      of_string(
        "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq",
      ),
    ).
      toBeNone()
  });
});
describe("contract_hash", ({test, _}) => {
  open Contract_hash;
  let kt1 = some_contract_hash;
  test("to_string", ({expect, _}) => {
    expect.string(to_string(kt1)).toEqual(
      "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc",
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc")).toBe(
      ~equals=Contract_hash.equal,
      Some(kt1),
    )
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(of_string("KT2Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc")).toBeNone()
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTd")).toBeNone()
  });
  test("invalid size", ({expect, _}) => {
    expect.option(of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABT")).toBeNone()
  });
});
describe("address", ({test, _}) => {
  open Address;

  let tz1 = Implicit(Key_hash.of_key(genesis_address));
  let kt1 = Originated({contract: some_contract_hash, entrypoint: None});
  let kt1_tuturu =
    Originated({contract: some_contract_hash, entrypoint: Some("tuturu")});
  test("to_string", ({expect, _}) => {
    expect.string(to_string(tz1)).toEqual(
      "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC",
    );
    expect.string(to_string(kt1)).toEqual(
      "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc",
    );
    expect.string(to_string(kt1_tuturu)).toEqual(
      "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc%tuturu",
    );
  });
  test("of_string", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBe(
      ~equals=Address.equal,
      Some(tz1),
    );
    // TODO: should we accept tz1 with entrypoint?
    // expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC%tuturu")).
    //   toBe(
    //
    //   ~equals=(==),
    //   Some(tz1),
    // );
    expect.option(of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc")).toBe(
      ~equals=Address.equal,
      Some(kt1),
    );
    expect.option(of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc%tuturu")).
      toBe(
      ~equals=Address.equal,
      Some(kt1_tuturu),
    );
    expect.option(of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc%default")).
      toBeNone();
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(of_string("tz4LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBeNone()
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLd")).toBeNone()
  });
  test("invalid size", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCL")).toBeNone()
  });
});
describe("ticket", ({test, _}) => {
  open Ticket;

  let kt1 =
    Address.Originated({contract: some_contract_hash, entrypoint: None});
  let ticket = {ticketer: kt1, data: Bytes.of_string("a")};
  test("to_string", ({expect, _}) => {
    expect.string(to_string(ticket)).toEqual(
      {|(Pair "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x61)|},
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(
      of_string({|(Pair "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x61)|}),
    ).
      toBe(
      ~equals=Ticket.equal,
      Some(ticket),
    )
  });

  test("invalid address", ({expect, _}) => {
    expect.option(
      of_string({|(Pair "BT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x61)|}),
    ).
      toBeNone()
  });
  test("invalid bytes", ({expect, _}) => {
    expect.option(
      of_string({|(Pair "BT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x6Z)|}),
    ).
      toBeNone()
  });
});
describe("operation_hash", ({test, _}) => {
  open Operation_hash;

  let op =
    of_string("opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4")
    |> Option.get;
  test("to_string", ({expect, _}) => {
    expect.string(to_string(op)).toEqual(
      "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4",
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(
      of_string({|opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4|}),
    ).
      toBe(
      ~equals=Operation_hash.equal,
      Some(op),
    )
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(
      of_string("obCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"),
    ).
      toBeNone()
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(
      of_string("opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW5"),
    ).
      toBeNone()
  });
  test("invalid size", ({expect, _}) => {
    expect.option(
      of_string("opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW"),
    ).
      toBeNone()
  });
});
