module String_map = Map.Make (String)

let file_descriptor_map = ref String_map.empty

let get_pipe_pair_file_descriptors ~is_chain path =
  match String_map.find_opt path !file_descriptor_map with
  | Some file_descriptors -> file_descriptors
  | None ->
      let vm_to_chain_path = path ^ "_read" in
      let chain_to_vm_path = path ^ "_write" in
      let vm_to_chain_permission, chain_to_vm_permission =
        if is_chain then ([ Unix.O_RDONLY ], [ Unix.O_WRONLY ])
        else ([ Unix.O_WRONLY ], [ Unix.O_RDONLY ])
      in
      let vm_to_chain =
        Unix.openfile vm_to_chain_path vm_to_chain_permission 0o666
      in
      let chain_to_vm =
        Unix.openfile chain_to_vm_path chain_to_vm_permission 0o666
      in
      file_descriptor_map :=
        String_map.add path (vm_to_chain, chain_to_vm) !file_descriptor_map;
      (vm_to_chain, chain_to_vm)

let make_pipe_pair path =
  let permissions = 0o600 in
  if not (Sys.file_exists (path ^ "_read")) then
    Unix.mkfifo (path ^ "_read") permissions;
  if not (Sys.file_exists (path ^ "_write")) then
    Unix.mkfifo (path ^ "_write") permissions
