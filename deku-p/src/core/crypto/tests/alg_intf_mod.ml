module Data_gen (Crypto : sig
  include Deku_crypto.Alg_intf.S

  type id = {
    public_key_hash : Key_hash.t;
    public_key : Key.t;
    secret_key : Secret.t;
  }

  val ids : id list
end)
(Tezos_data : Tezos_data.Tezos_data) =
struct
  open Deku_crypto
  open Crypto

  let get_encoding encoding =
    let encoding : bytes = Obj.magic encoding in
    Bytes.fold_left (fun acc char -> Char.code char :: acc) [] encoding
    |> List.rev

  module Secret_key_data = struct
    let secret_keys = List.map (fun id -> id.secret_key) ids
    let public_keys = List.map (fun sk -> Key.of_secret sk) secret_keys
    let compare_secret_keys = List.sort Secret.compare secret_keys

    let equality_secret_keys =
      List.for_all (fun sk -> Secret.equal sk sk) secret_keys

    let encoding = get_encoding Secret.encoding
  end

  module Key_data = struct
    (* These *)
    let public_keys = List.map (fun id -> id.public_key) ids

    let compare_public_keys =
      List.sort Key.compare (List.map (fun id -> id.public_key) ids)

    let equality_public_keys =
      List.for_all (fun pk -> Key.equal pk pk) public_keys

    let encoding = get_encoding Key.encoding
  end

  module Key_hash_data = struct
    let key_hashes = List.map (fun id -> id.public_key_hash) ids

    let compare_key_hash =
      List.sort Key_hash.compare (List.map (fun id -> id.public_key_hash) ids)

    let equality_key_hash =
      List.for_all (fun kh -> Key_hash.equal kh kh) key_hashes

    let encoding = get_encoding Key_hash.encoding
  end

  module Signature_data = struct
    let secret_keys = List.map (fun id -> id.secret_key) ids
    let public_keys = List.map (fun id -> id.public_key) ids
    let to_hash = [ "1"; "2"; "3"; "4"; "5" ]

    (* TODO: Print this for tests *)
    let to_sign = List.map (fun string -> BLAKE2b.hash string) to_hash

    let signatures =
      List.map
        (fun sk ->
          List.map (fun hash -> (Signature.sign sk hash, hash)) to_sign)
        secret_keys

    let signatures_to_b58 =
      List.map
        (fun sig_list ->
          List.map
            (fun (signature, hash) -> (Signature.to_b58 signature, hash))
            sig_list)
        signatures

    let b58_to_signatures =
      List.map
        (fun sig_list ->
          List.map
            (fun (b58, hash) -> (Option.get @@ Signature.of_b58 b58, hash))
            sig_list)
        signatures_to_b58

    let verified_normal_signatures =
      let check_sig pk signatures =
        List.map
          (fun (signature, hash) -> Signature.verify pk signature hash)
          signatures
      in
      List.map2
        (fun key signatures -> check_sig key signatures)
        public_keys signatures

    let verified_after_conversion =
      let check_sig pk signatures =
        List.map
          (fun (signature, hash) -> Signature.verify pk signature hash)
          signatures
      in
      List.map2
        (fun key signatures -> check_sig key signatures)
        public_keys b58_to_signatures

    let all_verified_normal =
      List.flatten verified_normal_signatures |> List.for_all Fun.id

    let all_verified_post_conversion =
      List.flatten verified_after_conversion |> List.for_all Fun.id

    let compare_signatures =
      List.sort compare
        (List.map (fun (signature, _) -> signature) (List.flatten signatures))

    let equality_signatures =
      List.for_all
        (fun (signature, _) -> Signature.equal signature signature)
        (List.flatten signatures)

    let encoding = get_encoding Signature.encoding
    let zero = Signature.(to_b58 zero)
    let size = Signature.size
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
          Secret_key_data.compare_secret_keys
      in
      Alcotest.(check' (list string))
        ~msg:"secret key comparison works"
        ~expected:Tezos_data.compared_secret_keys ~actual:compared_secret_keys

    let equality () =
      let equality_secret_key = Secret_key_data.equality_secret_keys in
      Alcotest.(check' bool)
        ~msg:"secret key equality works"
        ~expected:Tezos_data.equality_secret_keys ~actual:equality_secret_key

    let encoding () =
      let encoding = get_encoding Secret.encoding in
      Alcotest.(check' (list int))
        ~msg:"secret key encodings are equal"
        ~expected:Tezos_data.secret_key_encoding ~actual:encoding
  end

  module Test_key_data = struct
    let compare () =
      let compared_public_keys =
        List.map (fun pk -> Key.to_b58 pk) Key_data.compare_public_keys
      in
      Alcotest.(check' (list string))
        ~msg:"public key comparison works"
        ~expected:Tezos_data.compared_public_keys ~actual:compared_public_keys

    let equality () =
      let equality_public_keys = Key_data.equality_public_keys in
      Alcotest.(check' bool)
        ~msg:"public key equality works"
        ~expected:Tezos_data.equality_public_keys ~actual:equality_public_keys

    let encoding () =
      let encoding = get_encoding Key_data.encoding in
      Alcotest.(check' (list int))
        ~msg:"public key encodings are equal"
        ~expected:Tezos_data.public_key_encoding ~actual:encoding
  end

  module Test_key_hash_data = struct
    let compare () =
      let compare_key_hash =
        List.map (fun kh -> Key_hash.to_b58 kh) Key_hash_data.compare_key_hash
      in
      Alcotest.(check' (list string))
        ~msg:"key hash comparison works"
        ~expected:Tezos_data.compared_key_hashes ~actual:compare_key_hash

    let equality () =
      Alcotest.(check' bool)
        ~msg:"key hash equality works" ~expected:Tezos_data.equality_key_hash
        ~actual:Key_hash_data.equality_key_hash

    let encoding () =
      let encoding = get_encoding Key_hash.encoding in
      Alcotest.(check' (list int))
        ~msg:"key hash encodings are equal"
        ~expected:Tezos_data.public_key_hash_encoding ~actual:encoding
  end

  module Test_signature_data = struct
    let helper_print_signatures signature =
      let signature = Signature.to_b58 signature in
      let string_list =
        String.fold_left
          (fun acc char -> (Char.code char |> Int.to_string) :: acc)
          [] signature
      in
      String.concat "" string_list

    let signatures () =
      let signatures =
        let out =
          List.map
            (fun sig_list ->
              List.map (fun (sg, _) -> helper_print_signatures sg) sig_list)
            Signature_data.signatures
        in
        String.concat "" (List.flatten out)
      in
      Alcotest.(check' string)
        ~msg:"signatures are equal" ~expected:Tezos_data.signatures
        ~actual:signatures

    let verified_normal () =
      Alcotest.(check' (list (list bool)))
        ~msg:"verified normal signatures are equal"
        ~expected:Tezos_data.verified_normal_signatures
        ~actual:Signature_data.verified_normal_signatures

    let verified_after_conversion () =
      Alcotest.(check' (list (list bool)))
        ~msg:"verified post conversion signatures are equal"
        ~expected:Tezos_data.verified_after_conversion
        ~actual:Signature_data.verified_after_conversion

    let all_verified_normal () =
      Alcotest.(check' bool)
        ~msg:"all verified normal is equal"
        ~expected:Tezos_data.all_verified_normal
        ~actual:Signature_data.all_verified_normal

    let all_verified_post_conversion () =
      Alcotest.(check' bool)
        ~msg:"all verified post conversion is equal"
        ~expected:Tezos_data.all_verified_post_conversion
        ~actual:Signature_data.all_verified_post_conversion

    let compare () =
      let compare_signatures =
        List.map helper_print_signatures Signature_data.compare_signatures
        |> String.concat ""
      in
      Alcotest.(check' string)
        ~msg:"signature comparison works"
        ~expected:Tezos_data.compare_signatures ~actual:compare_signatures

    let equality () =
      Alcotest.(check' bool)
        ~msg:"signature equality works" ~expected:Tezos_data.equality_signatures
        ~actual:Signature_data.equality_signatures

    let encoding () =
      Alcotest.(check' (list int))
        ~msg:"signature encodings are equal"
        ~expected:Tezos_data.signature_encoding ~actual:Signature_data.encoding

    let zero () =
      Alcotest.(check' string)
        ~msg:"zeros are equal" ~expected:Tezos_data.zero
        ~actual:Signature_data.zero

    let size () =
      Alcotest.(check' int)
        ~msg:"sizes are equal" ~expected:Tezos_data.size
        ~actual:Signature_data.size
  end
end

(* TODO: Remove all useless variable declarations*)
