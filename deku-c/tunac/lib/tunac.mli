
type node = (int, string) Tezos_micheline.Micheline.node

type contract = node

type config =
  { debug : bool
  ; shared_memory : bool
  ; optimize : bool
  ; memory : int * int  }

val parse : string -> contract

val compile_contract : config:config -> contract -> (string, string) Lwt_result.t

val compile_value : node -> bytes

val link : string list -> string -> unit