open Setup;
open Protocol;
open Tezos_interop;

// TODO: maybe fuzz testing or any other cool testing magic?

describe("key", ({test, _}) => {
  open Key;

  // TODO: test encoding

  let edpk = Ed25519(Address.genesis_address);
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
      // TODO: proper equals
      ~equals=(==),
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
  let tz1 = of_key(Ed25519(Address.genesis_address));
  test("to_string", ({expect, _}) => {
    expect.string(to_string(tz1)).toEqual(
      "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC",
    )
  });
  test("of_string", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBe(
      // TODO: proper equals
      ~equals=(==),
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

  let edsk = Ed25519(Address.genesis_key);
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
      // TODO: proper equals
      ~equals=(==),
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

  let edpk = Key.Ed25519(Address.genesis_address);
  let edsk = Secret.Ed25519(Address.genesis_key);

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
      let (secret, key) = Mirage_crypto_ec.Ed25519.generate();
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
      // TODO: proper equals
      ~equals=(==),
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