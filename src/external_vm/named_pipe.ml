open Helpers
module String_map = Map.Make (String)

let file_descriptor_map = ref String_map.empty

let get_pipe_pair_file_descriptors path =
  match String_map.find_opt path !file_descriptor_map with
  | Some file_descriptors -> file_descriptors
  | None ->
    let read_path = path ^ "_read" in
    let write_path = path ^ "_write" in
    let read_fd = Unix.openfile read_path [Unix.O_RDONLY] 0o666 in
    let write_fd = Unix.openfile write_path [Unix.O_WRONLY] 0o666 in
    file_descriptor_map :=
      String_map.add path (read_fd, write_fd) !file_descriptor_map;
    (read_fd, write_fd)

let make_pipe_pair path =
  let permissions = 0o600 in
  if not (Sys.file_exists (path ^ "_read")) then
    Unix.mkfifo (path ^ "_read") permissions;
  if not (Sys.file_exists (path ^ "_write")) then
    Unix.mkfifo (path ^ "_write") permissions

let get_pipe_pair_channels path =
  let read_fd = Unix.openfile (path ^ "_read") [Unix.O_RDONLY] 0o000 in
  let read_channel = Lwt_io.of_unix_fd ~mode:Input read_fd in
  let write_fd = Unix.openfile (path ^ "_write") [Unix.O_WRONLY] 0o000 in
  let write_channel = Lwt_io.of_unix_fd ~mode:Output write_fd in
  (read_channel, write_channel)
