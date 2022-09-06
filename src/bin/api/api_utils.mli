val input_of_body :
  of_yojson:(Yojson.Safe.t -> 'a) -> Dream.request -> ('a, string) result Lwt.t
