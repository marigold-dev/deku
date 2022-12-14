open Deku_concepts
open Deku_protocol
open Deku_consensus

type fragment =
  | Producer_verify of { raw_operation : Protocol_operation.Raw.t }

type outcome =
  | Producer_verify_ok of {
      comb_wrapped_item : Protocol_payload.Wrapped_item.comb_wrapped_item;
    }
  (* TODO: more information? *)
  | Producer_verify_error

type status = Included | Pending of Protocol_payload.Wrapped_item.t

type producer =
  | Producer of {
      operations : status Protocol_operation.Raw.Hash.Map.t Level.Map.t;
    }

type t = producer

let empty = Producer { operations = Level.Map.empty }

let is_in_includable_window ~current_level raw_operation =
  let level = Protocol_operation.Raw.level raw_operation in
  Protocol_operation.Initial.is_in_includable_window ~current_level
    ~operation_level:level

let lookup_by_level level operations =
  match Level.Map.find_opt level operations with
  | Some map -> map
  | None -> Protocol_operation.Raw.Hash.Map.empty

let find ~hash ~level operations =
  let status_by_hash = lookup_by_level level operations in
  Protocol_operation.Raw.Hash.Map.find_opt hash status_by_hash

let add ~hash ~level status operations =
  let status_by_hash = lookup_by_level level operations in
  let status_by_hash =
    Protocol_operation.Raw.Hash.Map.add hash status status_by_hash
  in
  Level.Map.add level status_by_hash operations

let needs_to_consider_operation ~current_level raw_operation operations =
  match is_in_includable_window ~current_level raw_operation with
  | true -> (
      let hash = Protocol_operation.Raw.hash raw_operation in
      let level = Protocol_operation.Raw.level raw_operation in
      match find ~hash ~level operations with
      | Some (Pending _ | Included) -> false
      | None -> true)
  | false -> false

let incoming_raw_operation ~current_level raw_operation producer =
  let (Producer { operations }) = producer in
  match needs_to_consider_operation ~current_level raw_operation operations with
  | true -> Some (Producer_verify { raw_operation })
  | false -> None

(* TODO: this wraps it even if the operation is already included / known *)
let append ~current_level comb_wrapped_item producer =
  let open Protocol_payload.Wrapped_item in
  let (Producer { operations }) = producer in
  let (Comb_wrapped_item { wrapped_item; raw_operation }) = comb_wrapped_item in
  match needs_to_consider_operation ~current_level raw_operation operations with
  | true ->
      let hash = Protocol_operation.Raw.hash raw_operation in
      let level = Protocol_operation.Raw.level raw_operation in
      let status = Pending wrapped_item in
      let operations = add ~hash ~level status operations in
      Producer { operations }
  | false -> Producer { operations }

let compute fragment =
  match fragment with
  | Producer_verify { raw_operation } -> (
      (* TODO: we could reuse this work on the protocol,
          to make the producer faster *)
      match Protocol_operation.Raw.verify raw_operation with
      | Some _signed_operation -> (
          match Protocol_payload.Wrapped_item.encode raw_operation with
          | Some comb_wrapped_item -> Producer_verify_ok { comb_wrapped_item }
          | None ->
              (* TODO: this should be rejected on the network layer *)
              Producer_verify_error)
      | None -> Producer_verify_error)

let apply ~current_level ~outcome producer =
  match outcome with
  | Producer_verify_ok { comb_wrapped_item } ->
      append ~current_level comb_wrapped_item producer
  | Producer_verify_error -> producer

let tag_included ~current_level raw_operation producer =
  let (Producer { operations }) = producer in

  match is_in_includable_window ~current_level raw_operation with
  | true ->
      let hash = Protocol_operation.Raw.hash raw_operation in
      let level = Protocol_operation.Raw.level raw_operation in
      let operations = add ~hash ~level Included operations in
      Producer { operations }
  | false -> Producer { operations }

let produce ~identity ~above ~limit producer =
  let payload =
    let (Producer { operations }) = producer in
    (* TODO: better trimming *)
    let all_operations =
      Level.Map.fold
        (fun _level items operations ->
          Protocol_operation.Raw.Hash.Map.fold
            (fun _hash status operations ->
              (* TODO: this is not ideal *)
              match status with
              | Included -> operations
              | Pending item -> item :: operations)
            items operations)
        operations []
    in
    let rec rev_trim acc len list =
      match len >= limit with
      | true -> acc
      | false -> (
          match list with
          | [] -> acc
          | el :: tl -> rev_trim (el :: acc) (len + 1) tl)
    in
    let payload = rev_trim [] 0 all_operations in
    Protocol_payload.encode payload
  in
  let open Block in
  let (Block { hash = current_block; level = current_level; _ }) = above in
  let previous = current_block in
  let level = Level.next current_level in
  produce ~identity ~level ~previous ~payload

let drop ~current_level producer =
  (* TODO: this is O(1) but ugly, we could just drop the initial level,
      making it many times faster, does it matter? *)
  let (Producer { operations }) = producer in
  let operations =
    Level.Map.filter
      (fun operation_level _set ->
        Protocol_operation.Initial.is_in_includable_window ~current_level
          ~operation_level)
      operations
  in
  Producer { operations }

let encoding =
  let open Data_encoding in
  let status_encoding =
    union ~tag_size:`Uint8
      [
        case ~title:"Included" (Tag 0) unit
          (function Included -> Some () | Pending _ -> None)
          (fun () -> Included);
        case ~title:"Pending" (Tag 1) Protocol_payload.Wrapped_item.encoding
          (function
            | Pending wrapped_item -> Some wrapped_item | Included -> None)
          (fun wrapped_item -> Pending wrapped_item);
      ]
  in
  (* TODO: this is not safe, there is no coupling between wrapped_item
      in status and the level / hash here *)
  conv
    (fun (Producer { operations }) -> operations)
    (fun operations -> Producer { operations })
    (Level.Map.encoding
       (Protocol_operation.Raw.Hash.Map.encoding status_encoding))
