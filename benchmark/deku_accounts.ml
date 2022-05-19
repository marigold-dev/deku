open Bin_common

(*************************************************************************)
(* making Deku accounts - wallet *)

let make_wallet key_hash secret =
  {
    Files.Wallet.address = Crypto.Key_hash.of_string key_hash |> Option.get;
    Files.Wallet.priv_key = Crypto.Secret.of_string secret |> Option.get;
  }

(* Currently hardcode the addresses
   In sandbox.sh, alice_wallet is the deku_address   
*)
let alice_wallet =
  make_wallet "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf"
    "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"

let bob_wallet =
  make_wallet "tz1h1oFuYsCorjxekQ59bUe1uDGhuYvEx9ob"
    "edsk326F1xfCvHFw1LWhgtrwcm6DnFoHCmjjWX4vcWsJCbqmujJQVs"
