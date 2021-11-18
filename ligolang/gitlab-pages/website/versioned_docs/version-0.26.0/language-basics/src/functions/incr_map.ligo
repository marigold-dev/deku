function increment (const b : int) : int is
   (function (const a : int) : int is a + 1) (b)

function incr_map (const l : list (int)) : list (int) is
List.map (function (const i : int) : int is i + 1, l)
