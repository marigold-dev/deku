
let link_contract objfiles output =
  let objfiles = String.concat " " objfiles in
  let ret = Sys.command ("wasm-ld -o " ^ output ^ " --export=__michelson_stack --import-undefined " ^ objfiles) in
  match ret with
  | 0 -> ()
  | _ -> failwith "Couldn't run linker"