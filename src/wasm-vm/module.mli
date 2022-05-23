type t = Wasm.Ast.module_
val of_string : gas:int ref -> code:string -> (t, Errors.t) result
val encode : t -> (string, string) result
val decode : string -> (t, string) result
