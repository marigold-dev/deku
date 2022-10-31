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

  module Key_hash_data = struct
    let key_hashes = List.map (fun id -> id.public_key_hash) ids
    let compared_key_hashes = List.sort Key_hash.compare key_hashes
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

  module Test_key_hash_data = struct
    let compare () =
      let compared_key_hashes =
        List.map
          (fun kh -> Key_hash.to_b58 kh)
          Key_hash_data.compared_key_hashes
      in
      Alcotest.(check' (list string))
        ~msg:"key hash comparison works"
        ~expected:Tezos_data.compared_key_hashes ~actual:compared_key_hashes
  end
end
