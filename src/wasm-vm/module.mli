type t = Wasm.Ast.module_ [@@deriving yojson]
val of_string : code:string -> (t, Errors.t) result
val encode : t -> (string, string) result
val decode : string -> (t, Errors.t) result
