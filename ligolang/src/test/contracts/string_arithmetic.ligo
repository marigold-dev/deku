function length_op (const s : string) : nat is
  String.length (s)

function concat_op (const s : string) : string is
  String.concat (s, "toto")

function sub_op (const s : string) : string is
  String.sub (1n, 2n, s)
