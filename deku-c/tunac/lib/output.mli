type t = { module_ : string; constants : (int * Values.t) array }

val make :
  string ->
  (int * Values.t) array ->
  (t, [ `Invalid_module | `Module_validation_error ]) result
