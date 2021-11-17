{
(* initial version taken from https://github.com/realworldocaml/mdx *)
type arg = 
  | Field of string
  | NameValue of string * string

type block = {
  line    : int;
  file    : string;
  arguments: arg list;
  header  : string option;
  contents: string list;
}

exception Err of string

let line_ref = ref 1

let newline lexbuf =
  Lexing.new_line lexbuf;
  incr line_ref
}

let eol = '\n' | eof
let ws = ' ' | '\t'

rule text = parse
  | eof { [] }
  | "```" ([^' ' '\n']* as h) ws* ([^'\n']* as l) eol
      { 
        let header = if h = "" then None else Some h in
        let contents = block lexbuf in
        let arguments = String.split_on_char ' ' l in
        let arguments = List.map ~f:(fun a -> 
          if (String.contains a '=') then
            ( let a = String.split_on_char '=' a in
            NameValue (List.nth_exn a 0, List.nth_exn a 1))
          else 
            Field a
        ) arguments in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        newline lexbuf;
        let line = !line_ref in
        List.iter ~f: (fun _ -> newline lexbuf) contents;
        newline lexbuf;
        { file; line; header; arguments; contents; }
        :: text lexbuf }
  | [^'\n']* eol
      { newline lexbuf;
        text lexbuf }

and block = parse
  | eof | "```" ws* eol    { [] }
  | ([^'\n'] * as str) eol { str :: block lexbuf }

{
let token lexbuf =
  try
    text lexbuf
  with Failure _ -> 
    raise (Err "incomplete code block")
}
