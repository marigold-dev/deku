module Test_gen (Crypto : sig
  include Deku_crypto.Alg_intf.S

  type id = {
    public_key_hash : Key_hash.t;
    public_key : Key.t;
    secret_key : Secret.t;
  }

  val ids : id list
end)
(Tezos_data : Tezos_test_data.Tezos_data) =
struct
  open Crypto

  module Secret_key_data = struct
    let secret_keys = List.map (fun id -> id.secret_key) ids
    let public_keys = List.map (fun sk -> Key.of_secret sk) secret_keys
    let compared_secret_keys = List.sort Secret.compare secret_keys

    let equality_secret_keys =
      List.for_all (fun sk -> Secret.equal sk sk) secret_keys
  end

  module Test_secret_key_data = struct
    let public_keys () =
      let public_keys =
        List.map (fun pk -> Key.to_b58 pk) Secret_key_data.public_keys
      in
      Alcotest.(check' (list string))
        ~msg:"public keys are equal" ~expected:Tezos_data.public_keys
        ~actual:public_keys

    let compare () =
      let compared_secret_keys =
        List.map
          (fun sk -> Secret.to_b58 sk)
          Secret_key_data.compared_secret_keys
      in
      Alcotest.(check' (list string))
        ~msg:"secret key comparison works"
        ~expected:Tezos_data.compared_secret_keys ~actual:compared_secret_keys

    let equality () =
      Alcotest.(check' bool)
        ~msg:"secret key equality works"
        ~expected:Tezos_data.equality_secret_keys
        ~actual:Secret_key_data.equality_secret_keys
  end
end
