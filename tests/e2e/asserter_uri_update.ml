(* TODO: Make e2e tests to verify whether the uri of validator is properly updated in discovery contract *)

open Cmdliner


let assert_uri_update uri storage_uri =
  assert (uri = storage_uri);
  Format.printf "The uri is updated properly 👍 \n" 
 
  
let args =
  let uri =
    let docv = "uri" in
    let doc = "The updated uri of a particular validator" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let storage_uri = 
    let docv = "storage_uri" in
    let doc = "The uri of a particular validator is got in the storage of discovery contract" in
    let open Arg in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let open Term in
  const assert_uri_update $ uri $ storage_uri 
  
  let _ = Cmd.eval @@ Cmd.v (Cmd.info "asserter") args
