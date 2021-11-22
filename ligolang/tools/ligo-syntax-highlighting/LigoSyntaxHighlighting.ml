open Cmdliner

let vscode_directory =
  let doc = "Output VSCode files at given directory." in
  Arg.(required & opt (some string) None & info ["vscode"] ~doc)

let vim_directory =
  let doc = "Output VIM files at given directory." in
  Arg.(required & opt (some string) None & info ["vim"] ~doc)

let emacs_directory =
  let doc = "Output Emacs files at given directory." in
  Arg.(required & opt (some string) None & info ["emacs"] ~doc)

let output_file output_directory file s =
  let oc = (open_out @@ Filename.concat output_directory file) in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%s" s;
  close_out oc

let vim_syntax_highlighting dir file textmate = 
  let vim_output = SyntaxHighlighting.VIM.to_vim textmate in
  output_file dir file vim_output;
  `Ok "Success"

let vscode_syntax_highlighting: string -> string -> string -> string -> SyntaxHighlighting.Core.t -> string Term.ret = fun dir syntax_file language_file syntax textmate ->
  let jsons = SyntaxHighlighting.Textmate.to_jsons syntax textmate in
  match jsons with 
    Ok (syntax_highlighting_json, language_conf_json) -> 
      let s = Yojson.Safe.pretty_to_string syntax_highlighting_json in
      output_file dir syntax_file s;
      let s = Yojson.Safe.pretty_to_string language_conf_json in
      output_file dir language_file s;
      `Ok "Success" 
  | Error SyntaxHighlighting.Core.Referenced_rule_does_not_exist s -> `Error (false, Format.sprintf "Referenced rule '%s' does not exist." s)
  | Error Meta_name_some_but_empty s -> `Error (false, Format.sprintf  "%s.name has no value, but is expected to." s)
  | Error Begin_cant_be_empty s -> `Error (false, Format.sprintf  "%s.begin_ can't be empty." s)
  | Error End_cant_be_empty s -> `Error (false, Format.sprintf  "%s.end_ can't be empty" s)

let emacs_syntax_highlighting dir file textmate =
  let emacs_output = SyntaxHighlighting.Emacs.to_emacs textmate in
  output_file dir file emacs_output;
  `Ok "Success"

let ( let* ) o f : string Term.ret  =
  match o with
  | `Error _ as e -> e
  | `Help _ as h -> h
  | `Ok x -> f x

let output: string -> string -> string -> _ Term.ret = fun vscode_directory vim_directory emacs_directory ->
  if not (Sys.is_directory vscode_directory) then 
    `Error (false, "Not a valid directory to output VSCode files")
  else if not (Sys.is_directory vim_directory) then
    `Error (false, "Not a valid directory to output VIM files")
  else if not (Sys.is_directory emacs_directory) then
    `Error (false, "Not a valid directory to output EMacs files")
  else (
    let* _ = vscode_syntax_highlighting vscode_directory "ligo.tmLanguage.json" "ligo.configuration.json" "ligo" PascaLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "ligo.vim" PascaLIGO.syntax_highlighting in
    let* _ = emacs_syntax_highlighting emacs_directory "ligo-mode.el" PascaLIGO.syntax_highlighting in
    let* _ = vscode_syntax_highlighting vscode_directory "mligo.tmLanguage.json"  "mligo.configuration.json" "mligo" CameLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "mligo.vim" CameLIGO.syntax_highlighting in
    let* _ = emacs_syntax_highlighting emacs_directory "mligo-mode.el" CameLIGO.syntax_highlighting in
    let* _ = vscode_syntax_highlighting vscode_directory "religo.tmLanguage.json" "religo.configuration.json" "religo" ReasonLIGO.syntax_highlighting in
    let* _ = vim_syntax_highlighting vim_directory "religo.vim" ReasonLIGO.syntax_highlighting in
    let* _ = emacs_syntax_highlighting emacs_directory "religo-mode.el" ReasonLIGO.syntax_highlighting in
    `Ok "Successfully generated syntaxes"  
  )

let generate_syntax_highlighting =
  let doc = "generate syntax highlighting" in
  let exits = Term.default_exits in
  Term.(ret (const output $ vscode_directory $ vim_directory $ emacs_directory)), Term.info "LigoSyntaxHighlighting" ~exits ~doc

let () = Term.(exit @@ eval generate_syntax_highlighting)
