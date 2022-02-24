open Crypto
open Data_encoding

let () = Printexc.record_backtrace true

type transaction = {
  amount : Tez.t;
  destination : Address.t;
  entrypoint : string;
  value : Michelson.t;
}
type content = Transaction of transaction
type t = {
  source : Key_hash.t;
  fee : Tez.t;
  counter : Z.t;
  content : content;
  gas_limit : Gas.integral;
  storage_limit : Z.t;
}

let encoding =
  let case tag name args proj inj =
    case tag
      ~title:(String.capitalize_ascii name)
      (merge_objs (obj1 (req "kind" (constant name))) args)
      (fun x ->
        match proj x with
        | None -> None
        | Some x -> Some ((), x))
      (fun ((), x) -> inj x) in
  let operation_header_encoding =
    obj5
      (req "source" Key_hash.encoding)
      (req "fee" Tez.encoding)
      (req "counter" (check_size 10 n))
      (req "gas_limit" (check_size 10 Gas.n_integral_encoding))
      (req "storage_limit" (check_size 10 n)) in

  let make_operation_case ~tag ~name ~encoding to_ from =
    case (Tag tag) name
      (merge_objs operation_header_encoding encoding)
      (fun { source; fee; counter; gas_limit; storage_limit; content } ->
        let header = (source, fee, counter, gas_limit, storage_limit) in
        match to_ content with
        | Some content -> Some (header, content)
        | None -> None)
      (fun ((source, fee, counter, gas_limit, storage_limit), content) ->
        let content = from content in
        { source; fee; counter; gas_limit; storage_limit; content }) in

  let module Transaction = struct
    let entrypoint_encoding =
      def ~title:"entrypoint"
        ~description:"Named entrypoint to a Michelson smart contract"
        "entrypoint"
      @@
      let builtin_case tag name =
        Data_encoding.case (Tag tag) ~title:name (constant name)
          (fun n -> if String.equal n name then Some () else None)
          (fun () -> name) in
      union
        [
          builtin_case 0 "default";
          builtin_case 1 "root";
          builtin_case 2 "do";
          builtin_case 3 "set_delegate";
          builtin_case 4 "remove_delegate";
          Data_encoding.case (Tag 255) ~title:"named" (Bounded.string 31)
            (fun s -> Some s)
            (fun s -> s);
        ]

    let encoding =
      obj3
        (req "amount" Tez.encoding)
        (req "destination" Address.contract_encoding)
        (opt "parameters"
           (obj2
              (req "entrypoint" entrypoint_encoding)
              (req "value" Michelson.expr_encoding)))

    let encoding =
      conv
        (fun { amount; destination; entrypoint; value } ->
          let parameters =
            if String.equal entrypoint "default" && Michelson.is_unit value then
              None
            else
              Some (entrypoint, value) in
          (amount, destination, parameters))
        (fun (amount, destination, parameters) ->
          let entrypoint, value =
            match parameters with
            | Some (entrypoint, value) -> (entrypoint, value)
            | None -> ("default", Michelson.unit) in
          { amount; destination; entrypoint; value })
        encoding

    let tag = 108
    let name = "transaction"
    let case =
      make_operation_case ~tag ~name ~encoding
        (fun (Transaction transaction) -> Some transaction)
        (fun transaction -> Transaction transaction)
  end in
  union ~tag_size:`Uint8 [Transaction.case]

let shell_header_encoding =
  def "operation.shell_header" ~description:"An operation's shell header."
  @@ obj1 (req "branch" Block_hash.encoding)
let contents_list_encoding = Variable.list encoding
let injection_encoding =
  merge_objs shell_header_encoding
    (obj1 (req "contents" contents_list_encoding))

(* TODO: those encodings only work in a single protocol *)
let optional_signature_encoding =
  let signature_encoding =
    (* TODO: this encoding clearly should not be here *)
    let raw_encoding =
      conv Signature.to_raw
        (* this is reasonable as this encoding is not used to decode *)
          (fun _ -> failwith "unreachable")
        (Fixed.string Signature.size) in
    let name = "Signature" in
    let title = "A Ed25519, Secp256k1 or P256 signature" in
    Encoding_helpers.make_encoding ~name ~title ~to_string:Signature.to_string
      ~of_string:Signature.of_string ~raw_encoding in
  conv
    (function
      | Some s -> s
      | None -> Signature.zero)
    (fun s -> if Signature.equal s Signature.zero then None else Some s)
    signature_encoding

let operation_data_encoding =
  def "operation.alpha.contents_and_signature"
  @@ obj2
       (req "contents" contents_list_encoding)
       (req "signature" optional_signature_encoding)

(* TODO: maybe memoize this function?*)
let next_operation_encoding next_protocol_hash =
  def "next_operation"
  @@ conv
       (fun (branch, contents, signature) ->
         ((), (branch, (contents, signature))))
       (fun ((), (branch, (contents, signature))) ->
         (branch, contents, signature))
       (merge_objs
          (obj1 (req "protocol" (constant next_protocol_hash)))
          (merge_objs
             (dynamic_size shell_header_encoding)
             (dynamic_size operation_data_encoding)))
let preapply_input_encoding next_protocol_hash =
  list (next_operation_encoding next_protocol_hash)

let forge ~secret ~branch ~operations =
  (* TODO: assert operations length >= 1 *)
  let operation_bytes =
    Data_encoding.Binary.to_bytes_exn injection_encoding (branch, operations)
  in

  let signature =
    (* https://gitlab.com/tezos/tezos/-/blob/4ec1780093f6e2b5a063af6afd0db297d854e9fe/src/lib_crypto/signature.ml#L621 *)
    let watermark = "\x03" in
    let operation_bytes = Bytes.to_string operation_bytes in
    let operation_bytes_with_watermark = watermark ^ operation_bytes in
    let hash = BLAKE2B.hash operation_bytes_with_watermark in
    Signature.sign secret hash in
  (operation_bytes, signature)

let make_preapply_json ~secret ~protocol ~branch ~operations =
  let protocol = Protocol_hash.to_string protocol in
  let _operation_bytes, signature = forge ~secret ~branch ~operations in
  Data_encoding.Json.construct
    (preapply_input_encoding protocol)
    [(branch, operations, Some signature)]

let forge ~secret ~branch ~operations =
  let operation_bytes, signature = forge ~secret ~branch ~operations in
  let operation_with_signature_bytes =
    Bytes.to_string operation_bytes ^ Signature.to_raw signature in
  let (`Hex operation_with_signature_hex) =
    Hex.of_string operation_with_signature_bytes in
  operation_with_signature_hex
