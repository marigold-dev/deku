type t =
  { module_ : string
  ; constants : (int * Values.t) array
  ; entrypoints : Path.t option
  }
[@@deriving yojson]

val make :
     string
  -> (int * Values.t) array
  -> Path.t option
  -> (t, [ `Invalid_module | `Module_validation_error ]) result
