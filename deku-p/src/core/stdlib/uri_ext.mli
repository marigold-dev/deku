include module type of struct
  include Uri
end

val cmdliner_converter :
  (string -> [> `Ok of t | `Error of string ]) * (Format.formatter -> t -> unit)
