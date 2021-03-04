open Setup;
open Protocol;

describe("protocol state", ({test: _, _}) => {
  let make_address = () => {
    open Mirage_crypto_pk;
    let key = Rsa.generate(~bits=2048, ());
    (key, Rsa.pub_of_priv(key));
  };

  let (key, address) = make_address();
  let Signed.{key, signature, data} =
    Operation.Side_chain.{
      nonce: 0l,
      block_height: 1L,
      source: Wallet.of_address(address),
      amount: Amount.of_int(10),
      kind: Freeze,
    }
    |> Signed.sign(~key);
  let signed_operation =
    Operation.Side_chain.Self_signed.verify(~key, ~signature, data)
    |> Result.get_ok;
  Operation.Side_chain.Self_signed.to_yojson(signed_operation)
  |> Yojson.Safe.pretty_print(Format.std_formatter);

  // let x = {|{
  //   "key":
  // }|};
  // test("side chain operation", )
  ();
});
