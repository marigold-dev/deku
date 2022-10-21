module Data_gen (Crypto : sig
  open Deku_crypto
  include Alg_intf.S

  type id = {
    public_key_hash : Key_hash.t;
    public_key : Key.t;
    secret_key : Secret.t;
  }

  val ids : id list
end) =
struct
  open Deku_crypto
  open Crypto

  let print_encoding encoding =
    let encoding : bytes = Obj.magic encoding in
    Format.printf "[\n%!";
    Bytes.iter (fun char -> Format.printf "%d;\n%!" (Char.code char)) encoding;
    Format.printf "]\n%!"

  module Skt_key = struct
    let secret_keys = List.map (fun id -> id.secret_key) ids
    let public_keys = List.map (fun sk -> Key.of_secret sk) secret_keys
    let compare_secret_keys = List.sort Secret.compare secret_keys

    let equality_secret_keys =
      List.for_all (fun sk -> Secret.equal sk sk) secret_keys

    (* These*)
    (* Encoding sent to standard output*)
  end

  module Ky = struct
    (* These *)
    let public_keys = List.map (fun id -> id.public_key) ids

    let compare_public_keys =
      List.sort Key.compare (List.map (fun id -> id.public_key) ids)

    let equality_public_keys =
      List.for_all (fun pk -> Key.equal pk pk) public_keys
  end

  module Ky_hash = struct
    let key_hashes = List.map (fun id -> id.public_key_hash) ids

    let compare_key_hash =
      List.sort Key_hash.compare (List.map (fun id -> id.public_key_hash) ids)

    let equality_key_hash =
      List.for_all (fun kh -> Key_hash.equal kh kh) key_hashes
  end

  module Sig = struct
    let secret_keys = List.map (fun id -> id.secret_key) ids
    let public_keys = List.map (fun id -> id.public_key) ids
    let to_hash = [ "1"; "2"; "3"; "4"; "5" ]
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

    let verified_normal =
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
      List.flatten verified_normal |> List.for_all Fun.id

    let all_verified_post_conversion =
      List.flatten verified_after_conversion |> List.for_all Fun.id

    let compare_signatures =
      List.sort compare
        (List.map (fun (signature, _) -> signature) (List.flatten signatures))

    let equality_signatures =
      List.for_all
        (fun (signature, _) -> Signature.equal signature signature)
        (List.flatten signatures)

    let zero = Signature.(to_b58 zero)
    let size = Signature.size
  end

  module Print_secret_key = struct
    let print_public_keys () =
      Format.printf "let public_keys = [\n%!";
      List.iter
        (fun pk -> Format.printf "\"%s\";\n%!" (Key.to_b58 pk))
        Skt_key.public_keys;
      Format.printf "]\n%!"

    let print_compare_secret_keys () =
      Format.printf "let compared_secret_keys = [\n%!";
      List.iter
        (fun sk -> Format.printf "\"%s\"\n%!;" (Secret.to_b58 sk))
        Skt_key.compare_secret_keys;
      Format.printf "]\n%!"

    let print_equality_secret_keys () =
      Format.printf "let equality_secret_keys = %b\n%!"
        Skt_key.equality_secret_keys

    let print_encoding () =
      Format.printf "let secret_key_encoding = ";
      print_encoding Secret.encoding
  end

  module Print_key = struct
    let print_compare_public_keys () =
      Format.printf "let compared_public_keys = [\n%!";
      List.iter
        (fun sk -> Format.printf "\"%s\"\n%!;" (Key.to_b58 sk))
        Ky.compare_public_keys;
      Format.printf "]\n%!"

    let print_equality_public_keys () =
      Format.printf "let equality_public_keys = %b\n%!" Ky.equality_public_keys

    let print_encoding () =
      Format.printf "let public_key_encoding = ";
      print_encoding Key.encoding
  end

  module Print_key_hash = struct
    let print_compare_key_hash () =
      Format.printf "let compared_key_hashes = [\n%!";
      List.iter
        (fun sk -> Format.printf "\"%s\"\n%!;" (Key_hash.to_b58 sk))
        Ky_hash.compare_key_hash;
      Format.printf "]\n%!"

    let print_equality_key_hash () =
      Format.printf "let equality_key_hash = %b\n%!" Ky_hash.equality_key_hash

    let print_encoding () =
      Format.printf "let public_key_hash_encoding = ";
      print_encoding Key_hash.encoding
  end

  module Print_signatures = struct
    let helper_print_signatures signature =
      let signature = Signature.to_b58 signature in
      let string_list =
        String.fold_left
          (fun acc char -> (Char.code char |> Int.to_string) :: acc)
          [] signature
      in
      String.concat "" string_list

    let print_signatures () =
      Format.printf "let signatures = \n%!";
      let out =
        List.map
          (fun sig_list ->
            List.map (fun (sg, _) -> helper_print_signatures sg) sig_list)
          Sig.signatures
      in
      let out = String.concat "" (List.flatten out) in
      Format.printf "\"%s\"\n%!" out

    let print_verified_normal () =
      Format.printf "let verified_normal_signatures = [\n%!";
      List.iter
        (fun sig_list ->
          Format.printf "[\n%!";
          List.iter (Format.printf "%b;\n%!") sig_list;
          Format.printf "];\n%!")
        Sig.verified_normal;
      Format.printf "]\n%!"

    let print_verified_after_conversion () =
      Format.printf "let verified_after_conversion = [\n%!";
      List.iter
        (fun sig_list ->
          Format.printf "[\n%!";
          List.iter (Format.printf "%b;\n%!") sig_list;
          Format.printf "];\n%!")
        Sig.verified_after_conversion;
      Format.printf "]\n%!"

    let print_all_verified_normal () =
      Format.printf "let all_verified_normal = %b\n%!" Sig.all_verified_normal

    let print_all_verified_post_conversion () =
      Format.printf "let all_verified_post_conversion = %b\n%!"
        Sig.all_verified_post_conversion

    let print_compare_signatures () =
      Format.printf "let compare_signatures = \n%!";
      let out = List.map helper_print_signatures Sig.compare_signatures in
      Format.printf "\"%s\"\n%!" (String.concat "" out)

    let print_equality_signatures () =
      Format.printf "let equality_signatures = %b\n%!" Sig.equality_signatures

    let print_encoding () =
      Format.printf "let signature_encoding = ";
      print_encoding Signature.encoding

    let print_zero () = Format.printf "let zero = \"%s\"\n%!" Sig.zero
    let print_size () = Format.printf "let size = %d" Sig.size
  end

  let spc () = Format.eprintf "\n%!"

  let print_all_the_data () =
    let print_secret_key () =
      let open Print_secret_key in
      print_public_keys ();
      spc ();
      print_compare_secret_keys ();
      spc ();
      print_equality_secret_keys ();
      spc ();
      print_encoding ()
    in
    let print_key () =
      let open Print_key in
      print_compare_public_keys ();
      spc ();
      print_equality_public_keys ();
      spc ();
      print_encoding ()
    in
    let print_key_hash () =
      let open Print_key_hash in
      print_compare_key_hash ();
      spc ();
      print_equality_key_hash ();
      spc ();
      print_encoding ()
    in
    let print_signatures () =
      let open Print_signatures in
      print_signatures ();
      spc ();
      print_verified_normal ();
      spc ();
      print_verified_after_conversion ();
      spc ();
      print_all_verified_normal ();
      spc ();
      print_all_verified_post_conversion ();
      spc ();
      print_compare_signatures ();
      spc ();
      print_equality_signatures ();
      spc ();
      print_encoding ();
      spc ();
      print_zero ();
      spc ();
      print_size ()
    in
    print_secret_key ();
    spc ();
    print_key ();
    spc ();
    print_key_hash ();
    spc ();
    print_signatures ()
end
