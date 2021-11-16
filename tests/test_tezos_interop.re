open Setup;
open Protocol;
open Wallet;
open Crypto;
open Tezos;
open Tezos_interop;

module TZ2_ex = {
  let sk =
    Secret.of_string("spsk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy")
    |> Option.get;
  let pk =
    Key.of_string("sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U")
    |> Option.get;
};

module TZ3_ex = {
  let sk =
    Secret.of_string("p2sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ")
    |> Option.get;

  let pk =
    Key.of_string("p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7")
    |> Option.get;
};

// TODO: maybe fuzz testing or any other cool testing magic?
let some_contract_hash =
  Contract_hash.of_string("KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc")
  |> Option.get;
describe("key", ({test, _}) => {
  open Key;

  // TODO: test encoding

  let edpk = genesis_wallet;
  test("to_string", ({expect, _}) => {
    expect.string(to_string(edpk)).toEqual(
      "edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6",
    );
    expect.string(to_string(TZ2_ex.pk)).toEqual(
      "sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U",
    );
    expect.string(to_string(TZ3_ex.pk)).toEqual(
      "p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7",
    );
  });
  test("of_string", ({expect, _}) => {
    expect.option(
      of_string("edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6"),
    ).
      toBe(
      ~equals=Key.equal,
      Some(edpk),
    );
    expect.option(
      of_string("sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"),
    ).
      toBe(
      ~equals=Key.equal,
      Some(TZ2_ex.pk),
    );
    expect.option(
      of_string("p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"),
    ).
      toBe(
      ~equals=Key.equal,
      Some(TZ3_ex.pk),
    );
  });
  test("invalid prefix", ({expect, _}) => {
    // TODO: this test would fail anyway becose of the checksum
    //       craft an example with different prefix and valid checksum
    // TODO: when this test fails it segfaults
    // see https://github.com/reasonml/reason-native/pull/263
    expect.option(
      of_string("edpuvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6"),
    ).
      toBeNone();
    expect.option(
      of_string("sddk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"),
    ).
      toBeNone();
    expect.option(
      of_string("p3pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"),
    ).
      toBeNone();
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(
      of_string("edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT5"),
    ).
      toBeNone();
    expect.option(
      of_string("sppk7dCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"),
    ).
      toBeNone();
    expect.option(
      of_string("p2pk67mCLEz2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"),
    ).
      toBeNone();
  });
  test("invalid size", ({expect, _}) => {
    // TODO: this test would fail anyway becose of the checksum
    //       craft an example with different size but valid checksum
    expect.option(
      of_string("edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT"),
    ).
      toBeNone();
    expect.option(
      of_string("sppk7Cim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"),
    ).
      toBeNone();
    expect.option(
      of_string("p2pk67mCLE2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"),
    ).
      toBeNone();
  });
});
describe("key_hash", ({test, _}) => {
  open Key_hash;

  // TODO: proper test of_key
  let tz1 = of_key(genesis_wallet);
  let tz2 = of_key(TZ2_ex.pk);
  let tz3 = of_key(TZ3_ex.pk);
  test("to_string", ({expect, _}) => {
    expect.string(to_string(tz1)).toEqual(
      "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC",
    );
    expect.string(to_string(tz2)).toEqual(
      "tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD",
    );
    expect.string(to_string(tz3)).toEqual(
      "tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y",
    );
  });
  test("of_string", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBe(
      ~equals=Key_hash.equal,
      Some(tz1),
    );
    expect.option(of_string("tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD")).toBe(
      ~equals=Key_hash.equal,
      Some(tz2),
    );
    expect.option(of_string("tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBe(
      ~equals=Key_hash.equal,
      Some(tz3),
    );
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(of_string("tzaLzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBeNone();
    expect.option(of_string("tz4LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD")).toBeNone();
    expect.option(of_string("tzdb8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBeNone();
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLA")).toBeNone();
    expect.option(of_string("tz2LcrhRoD1PHxUYHq2DyEUjayG1kfqeqLVD")).toBeNone();
    expect.option(of_string("tz3d8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBeNone();
  });
  test("invalid size", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCL")).toBeNone();
    expect.option(of_string("tz2LcShRD1PHxUYHq2DyEUjayG1kfqeqLVD")).toBeNone();
    expect.option(of_string("tz38hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBeNone();
  });
});
describe("secret", ({test, _}) => {
  open Secret;

  let edsk = genesis_key;
  let spsk = TZ2_ex.sk;
  let p2sk = TZ3_ex.sk;

  test("to_string", ({expect, _}) => {
    expect.string(to_string(edsk)).toEqual(
      "edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd",
    );
    expect.string(to_string(spsk)).toEqual(
      "spsk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy",
    );
    expect.string(to_string(p2sk)).toEqual(
      "p2sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ",
    );
  });
  test("of_string", ({expect, _}) => {
    expect.option(
      of_string("edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd"),
    ).
      toBe(
      ~equals=Secret.equal,
      Some(edsk),
    );
    expect.option(
      of_string("spsk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"),
    ).
      toBe(
      ~equals=Secret.equal,
      Some(spsk),
    );
    expect.option(
      of_string("p2sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"),
    ).
      toBe(
      ~equals=Secret.equal,
      Some(p2sk),
    );
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(
      of_string("edsa4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd"),
    ).
      toBeNone();
    expect.option(
      of_string("spdk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"),
    ).
      toBeNone();
    expect.option(
      of_string("p3sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"),
    ).
      toBeNone();
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(
      of_string("edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVb"),
    ).
      toBeNone();
    expect.option(
      of_string("spsk3LfH15dYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"),
    ).
      toBeNone();
    expect.option(
      of_string("p2sk3s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"),
    ).
      toBeNone();
  });
  test("invalid size", ({expect, _}) => {
    expect.option(
      of_string("edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSV"),
    ).
      toBeNone();
    expect.option(
      of_string("spsk3LfH15YByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"),
    ).
      toBeNone();
    expect.option(
      of_string("p2sk2s8wFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"),
    ).
      toBeNone();
  });
});
describe("signature", ({test, _}) => {
  open BLAKE2B;
  open Signature;

  let edpk = genesis_wallet;
  let edsk = genesis_key;

  let tuturu = hash("tuturu");
  let tuturu2 = hash("tuturu2");

  // TODO: proper test for sign
  let edsig = sign(edsk, tuturu);

  let sppk = TZ2_ex.pk;
  let spsk = TZ2_ex.sk;
  let spsig = sign(spsk, tuturu);

  let p2pk = TZ3_ex.pk;
  let p2sk = TZ3_ex.sk;
  let p2sig = sign(p2sk, tuturu);

  test("check", ({expect, _}) => {
    expect.bool(verify(edpk, edsig, tuturu)).toBeTrue();
    expect.bool(verify(sppk, spsig, tuturu)).toBeTrue();
    expect.bool(verify(p2pk, p2sig, tuturu)).toBeTrue();
  });
  test("invalid message", ({expect, _}) => {
    expect.bool(verify(edpk, edsig, tuturu2)).toBeFalse();
    expect.bool(verify(sppk, spsig, tuturu2)).toBeFalse();
    expect.bool(verify(p2pk, p2sig, tuturu2)).toBeFalse();
  });
  test("invalid key", ({expect, _}) => {
    let (secret, key) = {
      let (secret, key) = Ed25519.generate();
      (Secret.Ed25519(secret), Key.Ed25519(key));
    };
    let edsig_from_key = sign(secret, tuturu);
    expect.bool(verify(key, edsig_from_key, tuturu)).toBeTrue();
    expect.bool(verify(edpk, edsig_from_key, tuturu)).toBeFalse();
    let (secret, key) = {
      let (secret, key) = Crypto.Secp256k1.generate();
      (Secret.Secp256k1(secret), Key.Secp256k1(key));
    };
    let pksig_from_key = sign(secret, tuturu);
    expect.bool(verify(key, pksig_from_key, tuturu)).toBeTrue();
    expect.bool(verify(sppk, pksig_from_key, tuturu)).toBeFalse();
    let (secret, key) = {
      let (secret, key) = Crypto.P256.generate();
      (Secret.P256(secret), Key.P256(key));
    };
    let p2sig_from_key = sign(secret, tuturu);
    expect.bool(verify(key, p2sig_from_key, tuturu)).toBeTrue();
    expect.bool(verify(p2pk, p2sig_from_key, tuturu)).toBeFalse();
  });

  test("to_string", ({expect, _}) => {
    expect.string(to_string(edsig)).toEqual(
      "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4",
    );
    expect.string(to_string(spsig)).toEqual(
      "spsig1ao4VRH1rAXpfUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh",
    );
    expect.string(to_string(p2sig)).toEqual(
      "p2sigt13usEEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo",
    );
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
    );
    expect.option(
      of_string(
        "spsig1ao4VRH1rAXpfUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh",
      ),
    ).
      toBe(
      ~equals=Signature.equal,
      Some(spsig),
    );
    expect.option(
      of_string(
        "p2sigt13usEEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo",
      ),
    ).
      toBe(
      ~equals=Signature.equal,
      Some(p2sig),
    );
  });
  test("invalid prefix", ({expect, _}) => {
    expect.option(
      of_string(
        "edsiatp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4",
      ),
    ).
      toBeNone();
    expect.option(
      of_string(
        "spsdg1ao4VRH1rAXpfUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh",
      ),
    ).
      toBeNone();
    expect.option(
      of_string(
        "p3sigt13usEEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo",
      ),
    ).
      toBeNone();
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(
      of_string(
        "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq3",
      ),
    ).
      toBeNone();
    expect.option(
      of_string(
        "spsig1ao4VRH1rAXpaUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh",
      ),
    ).
      toBeNone();
    expect.option(
      of_string(
        "p2sigt13useEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo",
      ),
    ).
      toBeNone();
  });
  test("invalid size", ({expect, _}) => {
    expect.option(
      of_string(
        "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq",
      ),
    ).
      toBeNone();
    expect.option(
      of_string(
        "spsig1ao4VRH1rAXpfUDT67y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh",
      ),
    ).
      toBeNone();
    expect.option(
      of_string(
        "p2sigt13usEEBeyJkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo",
      ),
    ).
      toBeNone();
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

  let tz1 = Implicit(Key_hash.of_key(genesis_wallet));
  let tz2 = Implicit(Key_hash.of_key(TZ2_ex.pk));
  let tz3 = Implicit(Key_hash.of_key(TZ3_ex.pk));
  let kt1 = Originated({contract: some_contract_hash, entrypoint: None});
  let kt1_tuturu =
    Originated({contract: some_contract_hash, entrypoint: Some("tuturu")});
  test("to_string", ({expect, _}) => {
    expect.string(to_string(tz1)).toEqual(
      "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC",
    );
    expect.string(to_string(tz2)).toEqual(
      "tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD",
    );
    expect.string(to_string(tz3)).toEqual(
      "tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y",
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
    expect.option(of_string("tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD")).toBe(
      ~equals=Address.equal,
      Some(tz2),
    );
    expect.option(of_string("tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBe(
      ~equals=Address.equal,
      Some(tz3),
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
    expect.option(of_string("tz4LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC")).toBeNone();
    expect.option(of_string("td2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD")).toBeNone();
    expect.option(of_string("tzab8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBeNone();
  });
  test("invalid checksum", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLd")).toBeNone();
    expect.option(of_string("tz2LcShaoD1PHxUYHq2DyEUjayG1kfqeqLVD")).toBeNone();
    expect.option(of_string("tz3b8hdcRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBeNone();
  });
  test("invalid size", ({expect, _}) => {
    expect.option(of_string("tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCL")).toBeNone();
    expect.option(of_string("tz2LcShRoDPHxUYHq2DyEUjayG1kfqeqLVD")).toBeNone();
    expect.option(of_string("tz3b8hJRfJzz5ZNCTepE9kU1QnTrL8Acy3y")).toBeNone();
  });
});
describe("ticket", ({test, _}) => {
  open Ticket_id;

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
      ~equals=equal,
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
    key(genesis_wallet),
    "050a0000002100d00725159de904a28aaed9adb2320f95bd2117959e41c1c2377ac11045d18bd7",
  );
  test(
    "key(\"sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U\")",
    key(TZ2_ex.pk),
    "050a0000002201027643ad744d2e26125b38726114eadf9f5f75af61838e7dee1bb7dda9df1984fd",
  );
  test(
    "key(\"p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7\")",
    key(TZ3_ex.pk),
    "050a000000220203a4242cde26340f1eb943956bf1209c5247c76ff4ddf51eb8cf050a70312c9c19",
  );
  test(
    "key_hash(\"tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC\")",
    key_hash(Key_hash.of_key(genesis_wallet)),
    "050a00000015000ec89608700c0414159d93552ef9361cea96da13",
  );
  test(
    "key_hash(\"tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD\")",
    key_hash(Key_hash.of_key(TZ2_ex.pk)),
    "050a000000150186e2a0c1f9a83eb066b54c8a594b3af88445b395",
  );
  test(
    "key_hash(\"tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y\")",
    key_hash(Key_hash.of_key(TZ3_ex.pk)),
    "050a0000001502a2645b31bbb34a3a070378905f2b03a823b63227",
  );
  test(
    "address(\"tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC\")",
    address(Implicit(Key_hash.of_key(genesis_wallet))),
    "050a0000001600000ec89608700c0414159d93552ef9361cea96da13",
  );
  test(
    "address(\"tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD\")",
    address(Implicit(Key_hash.of_key(TZ2_ex.pk))),
    "050a00000016000186e2a0c1f9a83eb066b54c8a594b3af88445b395",
  );
  test(
    "address(\"tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y\")",
    address(Implicit(Key_hash.of_key(TZ3_ex.pk))),
    "050a000000160002a2645b31bbb34a3a070378905f2b03a823b63227",
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
  let key_hash_exn = s => Key_hash.of_string(s) |> Option.get;
  let address_exn = s => Address.of_string(s) |> Option.get;

  test("hash_validators", ({expect, _}) => {
    let hash =
      [
        key_hash_exn("tz1XoDYhrUJT4HtskbEUrJusHtFHx6ZXcemd"),
        key_hash_exn("tz1R1XF4NnYkiCxcVphdLTYokQiyL38rtSQF"),
        key_hash_exn("tz1d6QHk2oFzrYYasZWof8BU26D7jXAXeajv"),
        key_hash_exn("tz1da6gqyddChGTwzW5aUA3Bia7DaAXmtqAE"),
      ]
      |> hash_validators;
    let hash = BLAKE2B.to_string(hash);
    expect.string(hash).toEqual(
      "6d6ecacbc858e3a89d87f0d9bd76b0c11b07aa95191129104395d17c6c96d36b",
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
