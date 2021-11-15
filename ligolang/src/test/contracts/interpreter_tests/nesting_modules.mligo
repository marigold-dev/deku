module Bar = struct
  let y (x : int) = 5 + x
end

module Foo = struct
  module Bar = struct
    let x = 54
  end
  let x = 55
end

let y () = Foo.Bar.x

let foo () =
  let x = 1 in
  module Foo = struct
    let x = x
  end in
  Foo.x

module Foo = struct
  let x = 2
  let x () = x + Foo.Bar.x
end

let main (_, s : unit * int) : operation list * int =
  let v = foo () + Foo.x () + y () in
  ([] : operation list), (s + v)

let test =
  let (taddr, _, _) = Test.originate main 0 0tez in
  let c = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn c () 0tez in
  Test.log (Test.get_storage taddr)
