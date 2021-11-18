type parameter1 = {
  [@annot:AAA] fooA : nat;
  fooB : nat;
  [@annot:cCC] fooC : nat;
}

let main1 (_ : parameter1 * unit) : operation list * unit =
  (([] : operation list), ())

type parameter2 =
  | [@annot:AAA] FooA of nat
  | FooB of nat
  | [@annot:cCC] FooC of nat

let main2 (_ : parameter2 * unit) : operation list * unit =
  (([] : operation list), ())
