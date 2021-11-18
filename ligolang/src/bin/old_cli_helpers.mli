open Cmdliner

val return_result : ?warn:bool -> ?output_file:string -> (string*string,string*string) result -> unit Term.ret
