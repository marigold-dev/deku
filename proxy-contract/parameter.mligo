module Types = struct
  (* The updated contract info for the proxy contract *)
  type t = {
    key : key; (* The public key of the validator or bridge address *)
    address : address;
    signature : signature;
  }
end