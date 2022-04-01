(** Interface between a long lived Go process and an OCaml consumer *)

(* TODO: some code is duplicated from tezos_interop/long_lived_js_process.ml *)

open Helpers

exception Process_closed of Unix.process_status
exception Failed_to_parse_json of string * Yojson.Safe.t

(* enhance error messages *)
let () =
  let open Format in
  let pp_process_status fmt status =
    match status with
    | Unix.WEXITED content -> fprintf fmt "WEXITED %d" content
    | Unix.WSIGNALED content -> fprintf fmt "WSIGNALED %d" content
    | Unix.WSTOPPED content -> fprintf fmt "WSTOPPED %d" content in

  let printer = function
    | Process_closed status ->
      Some (asprintf "Process_closed (%a)" pp_process_status status)
    | Failed_to_parse_json (message, json) ->
      Some
        (asprintf "Failed_to_parse_json (%s, %a)" message
           (Yojson.Safe.pretty_print ~std:false)
           json)
    | _ -> None in
  Printexc.register_printer printer

(* intentional, any exception in this file should kill the node *)
let raise exn =
  (* TODO: https://github.com/marigold-dev/deku/issues/502 *)
  Format.eprintf "go_vm failure: %s\n%!" (Printexc.to_string exn);
  exit 1

let read_all fd length =
  let message = Bytes.create length in
  let pos = ref 0 in
  while length > !pos do
    let read = Unix.read fd message !pos length in
    pos := !pos + read
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
  Format.printf "Sending message: %s\n%!" (message |> Yojson.Safe.to_string);
  let message = Bytes.of_string (Yojson.Safe.to_string message) in
  let message_length = Bytes.create 8 in
  Bytes.set_int64_ne message_length 0 (Int64.of_int (Bytes.length message));
  let _ = Unix.write fd message_length 0 (Bytes.length message_length) in
  write_all fd message

let read_from_vm ~fd =
  let fd = fd in
  let message_length = Bytes.create 8 in
  let _ = Unix.read fd message_length 0 8 in
  let message_length = Bytes.get_int64_ne message_length 0 |> Int64.to_int in
  let message = read_all fd message_length |> Bytes.to_string in
  Format.printf "Got message from machine: %s\n%!" message;
  Yojson.Safe.from_string message

type ('a, 'b) t = {
  send : 'a -> unit;
  receive : unit -> 'b;
}

let spawn ~path ~named_pipe_path ~of_yojson ~to_yojson =
  let () = Named_pipe.make_pipe_pair named_pipe_path in
  let pid =
    Unix.create_process path
      [|path; named_pipe_path|]
      Unix.stdin Unix.stdout Unix.stderr in
   let promise = Lwt_unix.waitpid [ ] pid in
   Lwt.async (fun () ->
       let%await (_exit_code, status) = promise in
       raise (Process_closed status));
   let read, write = Named_pipe.get_pipe_pair_file_descriptors named_pipe_path in
   let send x = to_yojson x |> send_to_vm ~fd:write in
   let receive () =
     let json = read_from_vm ~fd:read in
     match of_yojson json with
     | Ok x -> x
     | Error error -> raise (Failed_to_parse_json (error, json)) in
   { send; receive }
