
exception Error_compilation
val return_result : ?warn:bool -> ?output_file:string -> (string*string,string*string) result -> unit Proto_alpha_utils.Error_monad.tzresult Lwt.t
