module Test_gen (Crypto : sig
  include Deku_crypto.Alg_intf.S

  type id = {
    public_key_hash : Key_hash.t;
    public_key : Key.t;
    secret_key : Secret.t;
  }

  val ids : id list
end)
(Tezos_data : Tezos_test_data.Tezos_data) : sig
  val run : unit -> unit
end = struct
  open Crypto

  module Secret_key_data = struct
    let secret_keys = List.map (fun id -> id.secret_key) ids
    let public_keys = List.map (fun sk -> Key.of_secret sk) secret_keys
  end

  module Test_secret_key_data = struct
    let public_keys () =
      let public_keys =
        List.map (fun pk -> Key.to_b58 pk) Secret_key_data.public_keys
      in
      Alcotest.(check' (list string))
        ~msg:"public keys are equal" ~expected:Tezos_data.public_keys
        ~actual:public_keys
  end

  let run () = Test_secret_key_data.public_keys ()
end
