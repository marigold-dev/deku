type error =
  | Json_error         of string
  | Piaf_body          of Piaf.Error.t
  | Piaf_request       of Piaf.Error.t
  | Response_of_yojson of string

exception Request_parsing_error of error
