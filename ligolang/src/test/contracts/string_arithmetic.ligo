function concat_op (const s : string) : string is
  string_concat (s, "toto")

function slice_op (const s : string) : string is
  string_slice (1n, 2n, s)
