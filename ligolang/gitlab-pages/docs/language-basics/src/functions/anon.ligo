function increment (const b : int) : int is
   (function (const a : int) : int is a + 1) (b)

const a : int = increment (1); // a = 2
