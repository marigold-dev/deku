(** Interface between a long lived Go process and an OCaml consumer *)

(* TODO: some code is duplicated from tezos_interop/long_lived_js_process.ml *)

exception Process_closed of Unix.process_status
exception Failed_to_parse_json of string * Yojson.Safe.t

(* enhance error messages *)
let () =
  let open Format in
  let pp_process_status fmt status =
    match status with
    | Unix.WEXITED content -> fprintf fmt "WEXITED %d" content
    | Unix.WSIGNALED content -> fprintf fmt "WSIGNALED %d" content
    | Unix.WSTOPPED content -> fprintf fmt "WSTOPPED %d" content
  in

  let printer = function
    | Process_closed status ->
        Some (asprintf "Process_closed (%a)" pp_process_status status)
    | Failed_to_parse_json (message, json) ->
        Some
          (asprintf "Failed_to_parse_json (%s, %a)" message
             (Yojson.Safe.pretty_print ~std:false)
             json)
    | _ -> None
  in
  Printexc.register_printer printer

(* intentional, any exception in this file should kill the node *)
let raise exn =
  (* TODO: https://github.com/marigold-dev/deku/issues/502 *)
  Logs.err (fun m -> m "external_vm failure: %s" (Printexc.to_string exn));
  exit 1

let read_all fd lengtht =
  let message = Bytes.create lengtht in
  let length = ref lengtht in
  let pos = ref 0 in
  while lengtht > !pos do
    let read = Unix.read fd message !pos !length in
    pos := !pos + read;
    length := !length - read
  done;
  message

let write_all fd bytes_ =
  let bytes_len = Bytes.length bytes_ in
  let remaining = ref bytes_len in
  while !remaining > 0 do
    let pos = Bytes.length bytes_ - !remaining in
    let wrote = Unix.write fd bytes_ pos bytes_len in
    remaining := !remaining - wrote
  done

let send_to_vm ~fd (message : Yojson.Safe.t) =
  let message = Bytes.of_string (Yojson.Safe.to_string message) in
  let message_length = Bytes.create 8 in
  Bytes.set_int64_ne message_length 0 (Int64.of_int (Bytes.length message));
  let _ = Unix.write fd message_length 0 8 in
  write_all fd message

let read_from_vm ~fd =
  let fd = fd in
  let message_length = Bytes.create 8 in
  let _ = Unix.read fd message_length 0 8 in
  let message_length = Bytes.get_int64_ne message_length 0 |> Int64.to_int in
  let message = read_all fd message_length |> Bytes.to_string in
  Yojson.Safe.from_string message

type ('a, 'b) t = {
  send : 'a -> unit;
  receive : unit -> 'b;
  close : unit -> unit;
}

let open_pipes ~named_pipe_path ~of_yojson ~to_yojson ~is_chain =
  let () = Named_pipe.make_pipe_pair named_pipe_path in
  let vm_to_chain, chain_to_vm =
    Named_pipe.get_pipe_pair_file_descriptors ~is_chain named_pipe_path
  in
  let read, write =
    if is_chain then (vm_to_chain, chain_to_vm) else (chain_to_vm, vm_to_chain)
  in
  let send x = to_yojson x |> send_to_vm ~fd:write in
  let receive () =
    let json = read_from_vm ~fd:read in
    (* FIXME: what to do if this fails? *)
    of_yojson json
  in
  let close () = send_to_vm ~fd:write (`String "close") in
  { send; receive; close }

let open_vm_pipes ~named_pipe_path ~of_yojson ~to_yojson =
  open_pipes ~named_pipe_path ~of_yojson ~to_yojson ~is_chain:true

let open_chain_pipes ~named_pipe_path ~of_yojson ~to_yojson =
  open_pipes ~named_pipe_path ~of_yojson ~to_yojson ~is_chain:false
