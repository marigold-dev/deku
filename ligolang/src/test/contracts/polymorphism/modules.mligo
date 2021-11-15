module Bytes_ = struct
  let pack_ (type a) (x : a) : bytes = Bytes.pack x
  let unpack_ (type a) (x : bytes) : a option = Bytes.unpack x
end

module Foo = Bytes_

let packer (type b) (x : b) : bytes = Foo.pack_ x

let foo = (packer 1, packer "hello")

let bar =
  let (x, y) = foo in
  ((Bytes_.unpack_ x : int option), (Bytes_.unpack_ y : string option))
