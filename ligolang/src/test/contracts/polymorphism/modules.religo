module Bytes_ = {
  let pack_ : _a => bytes = (x : _a) => Bytes.pack(x)
  let unpack_ : bytes => option (_a) = (x : bytes) => (Bytes.unpack(x) : option (_a))
}

module Foo = Bytes_

let packer : _b => bytes = (x : _b) => Foo.pack_(x)

let foo = (packer(1), packer("hello"))

let bar =
  let (x, y) = foo;
  ((Bytes_.unpack_(x) : option(int)), (Bytes_.unpack_(y) : option(string)))
