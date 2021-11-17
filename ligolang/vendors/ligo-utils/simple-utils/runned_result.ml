type failwith =
  | Failwith_int of int
  | Failwith_string of string
  | Failwith_bytes of bytes

type 'a runned_result =
  | Success of 'a
  | Fail of failwith

type check_type = Check_parameter | Check_storage