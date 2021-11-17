let id_string = (p : string) : option(string) => {
  let packed : bytes = Bytes.pack (p);
  ((Bytes.unpack (packed)) : option (string));
};

let id_int = (p : int) : option (int) => {
  let packed : bytes = Bytes.pack (p);
  ((Bytes.unpack (packed)) : option (int));
};

let id_address = (p : address) : option (address) => {
  let packed : bytes = Bytes.pack (p);
  ((Bytes.unpack (packed)) : option (address));
};
