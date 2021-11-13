(* Converting the textual representation of tokens produced by Menhir
   into concrete syntax *)

module Make (Token : Token.S) =
  struct
    let unlex (sentence: string) : Buffer.t =
      let tokens  = Str.split (Str.regexp " ") sentence in
      let lexemes = List.map Token.concrete tokens in
      let buffer  = Buffer.create 31 in

      let rec trans = function
        [] -> ()
      |  [s] -> Buffer.add_string buffer s
      | s::l -> Buffer.add_string buffer (s ^ " "); trans l
      in trans lexemes; buffer
  end
