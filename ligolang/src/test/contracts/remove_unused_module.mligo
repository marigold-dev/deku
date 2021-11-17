module Baz = struct
  let g (x : int) = x
end


module Foo = struct
  type t = int
  let f (x : int) =  x
  module Bar = struct
    let g (x : int) = Baz.g x
  end
end

module Bah = Foo.Bar

module Foo = struct
  type t = int
  let f (x : int) = let () = Test.log "hello" in x
end

let main(_ : unit * unit) : operation list * unit =
  let _ = Bah.g 42 in
  ([] : operation list), ()
