module Types = struct
  type uri = string
  type nonce = int

  (* The updated uri for the discovery contract *)
  type t = {
    key: key;
    uri: uri;
    nonce: nonce;
    signature: signature;
  }
end