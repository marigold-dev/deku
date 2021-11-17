type a_type is int

function increment (const b : int) : int is
   (function (const a : a_type) : int is a + 1) (b)