open Setup
open Protocol
open Wallet
open Crypto
open Tezos
open Tezos_interop
module TZ2_ex = struct
  let sk =
    Secret.of_string "spsk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"
    |> Option.get
  let pk =
    Key.of_string "sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"
    |> Option.get
end
module TZ3_ex = struct
  let sk =
    Secret.of_string "p2sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"
    |> Option.get
  let pk =
    Key.of_string "p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"
    |> Option.get
end
let some_contract_hash =
  Contract_hash.of_string "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" |> Option.get

let () =
  describe "key" (fun { test; _ } ->
      let open Key in
      let edpk = genesis_wallet in
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string edpk)).toEqual
            "edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6";
          (expect.string (to_string TZ2_ex.pk)).toEqual
            "sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U";
          (expect.string (to_string TZ3_ex.pk)).toEqual
            "p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7");
      test "of_string" (fun { expect; _ } ->
          (expect.option
             (of_string "edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6"))
            .toBe
            ~equals:Key.equal (Some edpk);
          (expect.option
             (of_string
                "sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"))
            .toBe
            ~equals:Key.equal (Some TZ2_ex.pk);
          (expect.option
             (of_string
                "p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"))
            .toBe
            ~equals:Key.equal (Some TZ3_ex.pk));
      test "invalid prefix" (fun { expect; _ } ->
          (expect.option
             (of_string "edpuvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "sddk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "p3pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"))
            .toBeNone
            ());
      test "invalid checksum" (fun { expect; _ } ->
          (expect.option
             (of_string "edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT5"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "sppk7dCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "p2pk67mCLEz2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"))
            .toBeNone
            ());
      test "invalid size" (fun { expect; _ } ->
          (expect.option
             (of_string "edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT"))
            .toBeNone
            ();
          (expect.option
             (of_string "sppk7Cim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U"))
            .toBeNone
            ();
          (expect.option
             (of_string "p2pk67mCLE2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7"))
            .toBeNone
            ()))
let () =
  describe "key_hash" (fun { test; _ } ->
      let open Key_hash in
      let tz1 = of_key genesis_wallet in
      let tz2 = of_key TZ2_ex.pk in
      let tz3 = of_key TZ3_ex.pk in
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string tz1)).toEqual
            "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC";
          (expect.string (to_string tz2)).toEqual
            "tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD";
          (expect.string (to_string tz3)).toEqual
            "tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y");
      test "of_string" (fun { expect; _ } ->
          (expect.option (of_string "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC"))
            .toBe ~equals:Key_hash.equal (Some tz1);
          (expect.option (of_string "tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBe ~equals:Key_hash.equal (Some tz2);
          (expect.option (of_string "tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBe ~equals:Key_hash.equal (Some tz3));
      test "invalid prefix" (fun { expect; _ } ->
          (expect.option (of_string "tzaLzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC"))
            .toBeNone ();
          (expect.option (of_string "tz4LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBeNone ();
          (expect.option (of_string "tzdb8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBeNone ());
      test "invalid checksum" (fun { expect; _ } ->
          (expect.option (of_string "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLA"))
            .toBeNone ();
          (expect.option (of_string "tz2LcrhRoD1PHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBeNone ();
          (expect.option (of_string "tz3d8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBeNone ());
      test "invalid size" (fun { expect; _ } ->
          (expect.option (of_string "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCL"))
            .toBeNone ();
          (expect.option (of_string "tz2LcShRD1PHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBeNone ();
          (expect.option (of_string "tz38hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBeNone ()))
let () =
  describe "secret" (fun { test; _ } ->
      let open Secret in
      let edsk = genesis_key in
      let spsk = TZ2_ex.sk in
      let p2sk = TZ3_ex.sk in
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string edsk)).toEqual
            "edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd";
          (expect.string (to_string spsk)).toEqual
            "spsk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy";
          (expect.string (to_string p2sk)).toEqual
            "p2sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ");
      test "of_string" (fun { expect; _ } ->
          (expect.option
             (of_string "edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd"))
            .toBe
            ~equals:Secret.equal (Some edsk);
          (expect.option
             (of_string "spsk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"))
            .toBe
            ~equals:Secret.equal (Some spsk);
          (expect.option
             (of_string "p2sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"))
            .toBe
            ~equals:Secret.equal (Some p2sk));
      test "invalid prefix" (fun { expect; _ } ->
          (expect.option
             (of_string "edsa4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd"))
            .toBeNone
            ();
          (expect.option
             (of_string "spdk3LfH15rYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"))
            .toBeNone
            ();
          (expect.option
             (of_string "p3sk2s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"))
            .toBeNone
            ());
      test "invalid checksum" (fun { expect; _ } ->
          (expect.option
             (of_string "edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVb"))
            .toBeNone
            ();
          (expect.option
             (of_string "spsk3LfH15dYByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"))
            .toBeNone
            ();
          (expect.option
             (of_string "p2sk3s8WFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"))
            .toBeNone
            ());
      test "invalid size" (fun { expect; _ } ->
          (expect.option
             (of_string "edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSV"))
            .toBeNone
            ();
          (expect.option
             (of_string "spsk3LfH15YByf7whY9YNAxS5ghpjzCr96jZ16Jt4pv2mnshf8Tcy"))
            .toBeNone
            ();
          (expect.option
             (of_string "p2sk2s8wFJdvL6J9JxY7R3tWwMnaqJotbcTFgXg3pSVNPvrxKL1AkQ"))
            .toBeNone
            ()))
let () =
  describe "signature" (fun { test; _ } ->
      let open BLAKE2B in
      let open Signature in
      let edpk = genesis_wallet in
      let edsk = genesis_key in
      let tuturu = hash "tuturu" in
      let tuturu2 = hash "tuturu2" in
      let edsig = sign edsk tuturu in
      let sppk = TZ2_ex.pk in
      let spsk = TZ2_ex.sk in
      let spsig = sign spsk tuturu in
      let p2pk = TZ3_ex.pk in
      let p2sk = TZ3_ex.sk in
      let p2sig = sign p2sk tuturu in
      test "check" (fun { expect; _ } ->
          (expect.bool (verify edpk edsig tuturu)).toBeTrue ();
          (expect.bool (verify sppk spsig tuturu)).toBeTrue ();
          (expect.bool (verify p2pk p2sig tuturu)).toBeTrue ());
      test "invalid message" (fun { expect; _ } ->
          (expect.bool (verify edpk edsig tuturu2)).toBeFalse ();
          (expect.bool (verify sppk spsig tuturu2)).toBeFalse ();
          (expect.bool (verify p2pk p2sig tuturu2)).toBeFalse ());
      test "invalid key" (fun { expect; _ } ->
          let secret, key =
            let secret, key = Ed25519.generate () in
            (Secret.Ed25519 secret, Key.Ed25519 key) in
          let edsig_from_key = sign secret tuturu in
          (expect.bool (verify key edsig_from_key tuturu)).toBeTrue ();
          (expect.bool (verify edpk edsig_from_key tuturu)).toBeFalse ();
          let secret, key =
            let secret, key = Crypto.Secp256k1.generate () in
            (Secret.Secp256k1 secret, Key.Secp256k1 key) in
          let pksig_from_key = sign secret tuturu in
          (expect.bool (verify key pksig_from_key tuturu)).toBeTrue ();
          (expect.bool (verify sppk pksig_from_key tuturu)).toBeFalse ();
          let secret, key =
            let secret, key = Crypto.P256.generate () in
            (Secret.P256 secret, Key.P256 key) in
          let p2sig_from_key = sign secret tuturu in
          (expect.bool (verify key p2sig_from_key tuturu)).toBeTrue ();
          (expect.bool (verify p2pk p2sig_from_key tuturu)).toBeFalse ());
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string edsig)).toEqual
            "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4";
          (expect.string (to_string spsig)).toEqual
            "spsig1ao4VRH1rAXpfUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh";
          (expect.string (to_string p2sig)).toEqual
            "p2sigt13usEEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo");
      test "of_string" (fun { expect; _ } ->
          (expect.option
             (of_string
                "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4"))
            .toBe
            ~equals:Signature.equal (Some edsig);
          (expect.option
             (of_string
                "spsig1ao4VRH1rAXpfUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh"))
            .toBe
            ~equals:Signature.equal (Some spsig);
          (expect.option
             (of_string
                "p2sigt13usEEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo"))
            .toBe
            ~equals:Signature.equal (Some p2sig));
      test "invalid prefix" (fun { expect; _ } ->
          (expect.option
             (of_string
                "edsiatp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq4"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "spsdg1ao4VRH1rAXpfUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "p3sigt13usEEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo"))
            .toBeNone
            ());
      test "invalid checksum" (fun { expect; _ } ->
          (expect.option
             (of_string
                "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq3"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "spsig1ao4VRH1rAXpaUDT6B7y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "p2sigt13useEBey8JkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo"))
            .toBeNone
            ());
      test "invalid size" (fun { expect; _ } ->
          (expect.option
             (of_string
                "edsigtp1tNe8hVhfn9QPoGaLyxqksCbRxk6w3wTjbMDyu4QckAyhMDwUQc3yDCjfSqFXeccLkRjE1c1Lbm71i7uhGZqx7V8nSq"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "spsig1ao4VRH1rAXpfUDT67y2ScbbM5RSP9ZRqtkKuKbE4Eo1QGAKqMVKieYtvFcUDUeqSpGWcqVMedfsfKr45yuQqWpz7Wgqh"))
            .toBeNone
            ();
          (expect.option
             (of_string
                "p2sigt13usEEBeyJkpRCarwHnMUZ744xeAQFv65ZT5UZeNgoGaFDdFfw27MEGpGSSAdxpSyBdfBhkrtUf7cwHx6NQnCpjWbpo"))
            .toBeNone
            ()))
let () =
  describe "contract_hash" (fun { test; _ } ->
      let open Contract_hash in
      let kt1 = some_contract_hash in
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string kt1)).toEqual
            "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc");
      test "of_string" (fun { expect; _ } ->
          (expect.option (of_string "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc"))
            .toBe ~equals:Contract_hash.equal (Some kt1));
      test "invalid prefix" (fun { expect; _ } ->
          (expect.option (of_string "KT2Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc"))
            .toBeNone ());
      test "invalid checksum" (fun { expect; _ } ->
          (expect.option (of_string "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTd"))
            .toBeNone ());
      test "invalid size" (fun { expect; _ } ->
          (expect.option (of_string "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABT"))
            .toBeNone ()))
let () =
  describe "address" (fun { test; _ } ->
      let open Address in
      let tz1 = Implicit (Key_hash.of_key genesis_wallet) in
      let tz2 = Implicit (Key_hash.of_key TZ2_ex.pk) in
      let tz3 = Implicit (Key_hash.of_key TZ3_ex.pk) in
      let kt1 =
        Originated { contract = some_contract_hash; entrypoint = None } in
      let kt1_tuturu =
        Originated { contract = some_contract_hash; entrypoint = Some "tuturu" }
      in
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string tz1)).toEqual
            "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC";
          (expect.string (to_string tz2)).toEqual
            "tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD";
          (expect.string (to_string tz3)).toEqual
            "tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y";
          (expect.string (to_string kt1)).toEqual
            "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc";
          (expect.string (to_string kt1_tuturu)).toEqual
            "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc%tuturu");
      test "of_string" (fun { expect; _ } ->
          (expect.option (of_string "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC"))
            .toBe ~equals:Address.equal (Some tz1);
          (expect.option (of_string "tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBe ~equals:Address.equal (Some tz2);
          (expect.option (of_string "tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBe ~equals:Address.equal (Some tz3);
          (expect.option (of_string "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc"))
            .toBe ~equals:Address.equal (Some kt1);
          (expect.option
             (of_string "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc%tuturu"))
            .toBe ~equals:Address.equal (Some kt1_tuturu);
          (expect.option
             (of_string "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc%default"))
            .toBeNone ());
      test "invalid prefix" (fun { expect; _ } ->
          (expect.option (of_string "tz4LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC"))
            .toBeNone ();
          (expect.option (of_string "td2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBeNone ();
          (expect.option (of_string "tzab8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBeNone ());
      test "invalid checksum" (fun { expect; _ } ->
          (expect.option (of_string "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLd"))
            .toBeNone ();
          (expect.option (of_string "tz2LcShaoD1PHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBeNone ();
          (expect.option (of_string "tz3b8hdcRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBeNone ());
      test "invalid size" (fun { expect; _ } ->
          (expect.option (of_string "tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCL"))
            .toBeNone ();
          (expect.option (of_string "tz2LcShRoDPHxUYHq2DyEUjayG1kfqeqLVD"))
            .toBeNone ();
          (expect.option (of_string "tz3b8hJRfJzz5ZNCTepE9kU1QnTrL8Acy3y"))
            .toBeNone ()))
let () =
  describe "ticket" (fun { test; _ } ->
      let open Ticket_id in
      let kt1 =
        Address.Originated { contract = some_contract_hash; entrypoint = None }
      in
      let ticket = { ticketer = kt1; data = Bytes.of_string "a" } in
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string ticket)).toEqual
            {|(Pair "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x61)|});
      test "of_string" (fun { expect; _ } ->
          (expect.option
             (of_string {|(Pair "KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x61)|}))
            .toBe
            ~equals:equal (Some ticket));
      test "invalid address" (fun { expect; _ } ->
          (expect.option
             (of_string {|(Pair "BT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x61)|}))
            .toBeNone
            ());
      test "invalid bytes" (fun { expect; _ } ->
          (expect.option
             (of_string {|(Pair "BT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc" 0x6Z)|}))
            .toBeNone
            ()))
let () =
  describe "operation_hash" (fun { test; _ } ->
      let open Operation_hash in
      let op =
        of_string "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"
        |> Option.get in
      test "to_string" (fun { expect; _ } ->
          (expect.string (to_string op)).toEqual
            "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4");
      test "of_string" (fun { expect; _ } ->
          (expect.option
             (of_string {|opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4|}))
            .toBe
            ~equals:Operation_hash.equal (Some op));
      test "invalid prefix" (fun { expect; _ } ->
          (expect.option
             (of_string "obCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"))
            .toBeNone
            ());
      test "invalid checksum" (fun { expect; _ } ->
          (expect.option
             (of_string "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW5"))
            .toBeNone
            ());
      test "invalid size" (fun { expect; _ } ->
          (expect.option
             (of_string "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW"))
            .toBeNone
            ()))
let () =
  describe "operation forging" (fun { test; _ } ->
      let open Tezos in
      let open Operation in
      let forge_transaction ~branch ~fee ~gas_limit ~storage_limit ~amount
          ~destination ~source ~counter ~secret ~entrypoint ~value =
        let branch = Block_hash.of_string branch |> Option.get in
        let fee = Tez.of_mutez fee |> Option.get in
        let gas_limit = Gas.of_int gas_limit |> Option.get in
        let storage_limit = Z.of_int storage_limit in
        let amount = Tez.of_mutez amount |> Option.get in
        let destination = Address.of_string destination |> Option.get in
        let source = Key_hash.of_string source |> Option.get in
        let counter = Z.of_int counter in
        let secret = Secret.of_string secret |> Option.get in
        let operation =
          {
            source;
            fee;
            counter;
            content = Transaction { amount; destination; entrypoint; value };
            gas_limit;
            storage_limit;
          } in
        Tezos.Operation.forge ~secret ~branch ~operations:[operation] in
      test "same result as taquito" (fun { expect; _ } ->
          let forged_bytes =
            forge_transaction
              ~branch:"BLBQQXyZ1qTwxZiT5FkJwvKj4YnCmx6xhqnhgaZg1Z13aQDJiCk"
              ~fee:443L ~gas_limit:1520 ~storage_limit:0 ~amount:2000000L
              ~destination:"tz1ULf5uGJXefx8c8iLfHfuW1doMPpVicg7u"
              ~source:"tz1M6iKVFN8RhHjVSL3oF75nF2FJ1yMkrk5t" ~counter:3305389
              ~secret:"edsk4RbgwutwsEdVNuJsE5JDsxeJ6qFcG8F5rKFGnj5finT6FV46sd"
              ~entrypoint:"default" ~value:Michelson.unit in
          let taquito_forged_bytes =
            "3d95683f0d29a6deb044f4ecd86efd9cbae6b373b7b9d7c2db16456783e664566c001004051072b588b39e25b9f4dbf4673abcd63147bb03addfc901f00b0080897a00005f70062003e798791cb04f51bcec1d3358ac64a600468a797b657cff8b585c18cb599443d7c0982220cfa9231270138c7a3e0996d8ea42a2912d51c44681877b2d3a3e8051ebd1498305f8f73cf68967f60349fc00"
          in
          (expect.string forged_bytes).toEqual taquito_forged_bytes);
      test "parameter unit and entrypoint default" (fun { expect; _ } ->
          let forged_bytes =
            forge_transaction
              ~branch:"BMb1r7vPdSkTb8ACDpuk4vKXPqEm6knqKjEzqpNj8Prxb2KWMP3"
              ~fee:420L ~gas_limit:1303 ~storage_limit:0 ~amount:0L
              ~destination:"KT1GAr6WWLeavRVHgxEJq1F7tNLzavCLu9YB"
              ~source:"tz1M6iKVFN8RhHjVSL3oF75nF2FJ1yMkrk5t" ~counter:3305396
              ~secret:"edsk4RbgwutwsEdVNuJsE5JDsxeJ6qFcG8F5rKFGnj5finT6FV46sd"
              ~entrypoint:"default" ~value:Michelson.unit in
          let taquito_forged_bytes =
            "f6e43992b2f45aedbfdc7f5f6a21aa68c99973afffa0ddbe8b98e33672dec6396c001004051072b588b39e25b9f4dbf4673abcd63147a403b4dfc901970a000001533ace00d71497d23fdac2a8809bb1e9df14c579000048a432efc5ef0e700c2f696957996e90194787995d43826b18ade1823d05857f35787f4e7a223d3ea226f22b1809ee56a9046bd313f547e3746fb46bc8ed3803"
          in
          (expect.string forged_bytes).toEqual taquito_forged_bytes))
let () =
  describe "pack" (fun { test; _ } ->
      let open Pack in
      let test name input output =
        test name (fun { expect; _ } ->
            let (`Hex result) = to_bytes input |> Hex.of_bytes in
            (expect.string result).toEqual output) in
      let int n = int (Z.of_int n) in
      let bytes s = bytes (Bytes.of_string (Hex.to_string (`Hex s))) in
      test "int(1)" (int 1) "050001";
      test "int(-1)" (int (-1)) "050041";
      test "bytes(0x)" (bytes "") "050a00000000";
      test "bytes(0x050001)" (bytes "050001") "050a00000003050001";
      test "pair(1, 0x)" (pair (int 1) (bytes "")) "05070700010a00000000";
      test "pair(1, (0xAA, -1))"
        (pair (int 1) (pair (bytes "AA") (int (-1))))
        "050707000107070a00000001aa0041";
      test "list([])" (list []) "050200000000";
      test "list([1])" (list [int 1]) "0502000000020001";
      test "list([(1, (0x, -1))])"
        (list [pair (int 1) (pair (bytes "") (int (-1)))])
        "05020000000d0707000107070a000000000041";
      test "key(\"edpkvDqjL7aXdsXSiK5ChCMAfqaqmCFWCv7DaT3dK1egJt136WBiT6\")"
        (key genesis_wallet)
        "050a0000002100d00725159de904a28aaed9adb2320f95bd2117959e41c1c2377ac11045d18bd7";
      test "key(\"sppk7aCim2kYBWGoEQGezcg8LbHi99XxrAE5dh6DDUru26pT93V2c5U\")"
        (key TZ2_ex.pk)
        "050a0000002201027643ad744d2e26125b38726114eadf9f5f75af61838e7dee1bb7dda9df1984fd";
      test "key(\"p2pk67mCLEZ2iFivprKpjugLakXfahdq9VUqmiTho1fhtv9P3AeJoc7\")"
        (key TZ3_ex.pk)
        "050a000000220203a4242cde26340f1eb943956bf1209c5247c76ff4ddf51eb8cf050a70312c9c19";
      test "key_hash(\"tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC\")"
        (key_hash (Key_hash.of_key genesis_wallet))
        "050a00000015000ec89608700c0414159d93552ef9361cea96da13";
      test "key_hash(\"tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD\")"
        (key_hash (Key_hash.of_key TZ2_ex.pk))
        "050a000000150186e2a0c1f9a83eb066b54c8a594b3af88445b395";
      test "key_hash(\"tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y\")"
        (key_hash (Key_hash.of_key TZ3_ex.pk))
        "050a0000001502a2645b31bbb34a3a070378905f2b03a823b63227";
      test "address(\"tz1LzCSmZHG3jDvqxA8SG8WqbrJ9wz5eUCLC\")"
        (address (Implicit (Key_hash.of_key genesis_wallet)))
        "050a0000001600000ec89608700c0414159d93552ef9361cea96da13";
      test "address(\"tz2LcShRoD1PHxUYHq2DyEUjayG1kfqeqLVD\")"
        (address (Implicit (Key_hash.of_key TZ2_ex.pk)))
        "050a00000016000186e2a0c1f9a83eb066b54c8a594b3af88445b395";
      test "address(\"tz3b8hJcRfJzz5ZNCTepE9kU1QnTrL8Acy3y\")"
        (address (Implicit (Key_hash.of_key TZ3_ex.pk)))
        "050a000000160002a2645b31bbb34a3a070378905f2b03a823b63227";
      test "address(\"KT1Dbav7SYrJFpd3bT7sVFDS9MPp4F5gABTc\")"
        (address
           (Originated { contract = some_contract_hash; entrypoint = None }))
        "050a0000001601370027c6c8f3fbafda4f9bfd08b14f45e6a29ce300")
let () =
  describe "consensus" (fun { test; _ } ->
      let open Helpers in
      let open Deku.Consensus in
      let hash_exn s = BLAKE2B.of_string s |> Option.get in
      let key_hash_exn s = Key_hash.of_string s |> Option.get in
      let address_exn s = Address.of_string s |> Option.get in
      test "hash_validators" (fun { expect; _ } ->
          let hash =
            [
              key_hash_exn "tz1XoDYhrUJT4HtskbEUrJusHtFHx6ZXcemd";
              key_hash_exn "tz1R1XF4NnYkiCxcVphdLTYokQiyL38rtSQF";
              key_hash_exn "tz1d6QHk2oFzrYYasZWof8BU26D7jXAXeajv";
              key_hash_exn "tz1da6gqyddChGTwzW5aUA3Bia7DaAXmtqAE";
            ]
            |> hash_validators in
          let hash = BLAKE2B.to_string hash in
          (expect.string hash).toEqual
            "6d6ecacbc858e3a89d87f0d9bd76b0c11b07aa95191129104395d17c6c96d36b");
      test "hash_block" (fun { expect; _ } ->
          let hash =
            hash_block ~block_height:121L
              ~block_payload_hash:
                (hash_exn
                   "2d92960a592c56de3046e200969c230a2eda71fc4b775e0cc09a189e5ddc5dbd")
              ~state_root_hash:
                (hash_exn
                   "bdd051ddb07925a0d88dc27583e38ae560aa1b4429cc93b9ec35dacdbd74ffb2")
              ~withdrawal_handles_hash:
                (hash_exn
                   "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8")
              ~validators_hash:
                (hash_exn
                   "546d2bb2375cc919efc81a103a7ad3bd1227546b320f275e357bd9a5d5eef946")
          in
          let hash = BLAKE2B.to_string hash in
          (expect.string hash).toEqual
            "7cb600c19817b899d4c28c521dd9ebf95f688e1444afe7d0e7740bebe848b030");
      test "hash_withdraw_handle" (fun { expect; _ } ->
          let hash =
            hash_withdraw_handle ~id:(Z.of_int 0)
              ~owner:(address_exn "tz1YywYq77UAMbVgoYndnZLkRawjUhX3nVh4")
              ~amount:(Z.of_int 10)
              ~ticketer:(address_exn "KT1AS9rCk1wpybsvZ5Tnd4yRxDvtN39uxMoq")
              ~data:(Bytes.of_string "") in
          let hash = BLAKE2B.to_string hash in
          (expect.string hash).toEqual
            "63dfd90ec7be98a9c23bf374692de4d36f41fe03c4c768fc0c650641d3ed4f86"))
let () =
  describe "discovery" (fun { test; _ } ->
      let open Discovery in
      let secret = genesis_key in
      test "sign" (fun { expect; _ } ->
          let signature =
            sign secret ~nonce:1L (Uri.of_string "http://localhost") in
          (expect.string (Signature.to_string signature)).toEqual
            "edsigtpGEA7XKPKMFkFiEA6SfJaaha4Ai2XbteJG5FYvuMtMRyPnXRuZNi54P7BWvV6GaWTijf8EBjGb8MZZvdTrWCGFCVCXL7r"))
