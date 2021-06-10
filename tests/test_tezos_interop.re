open Setup;
open Protocol;
open Tezos_interop;

describe("key", ({test, _}) => {
  open Key;

  // TODO: test encoding

  test("to_string", ({expect, _}) => {
    expect.string(to_string(Ed25519(Address.genesis_address))).toEqual(
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
      Some(Ed25519(Address.genesis_address)),
    )
  });
  test("invalid prefix", ({expect, _}) => {
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
    expect.option(
      of_string("edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT"),
    ).
      toBeNone()
  });
});
