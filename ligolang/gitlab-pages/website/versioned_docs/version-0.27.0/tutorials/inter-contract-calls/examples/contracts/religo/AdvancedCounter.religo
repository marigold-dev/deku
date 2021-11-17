type parameter = Set(int) | Add(int) | Subtract(int) | Multiply(int) | Reset;

let main = ((param, storage): (parameter, int)) => {
  let nop: list(operation) = [];
  switch(param){
  | Set n => (nop, n)
  | Add n => (nop, storage + n)
  | Subtract n => (nop, storage - n)
  | Multiply n => (nop, storage * n)
  | Reset => (nop, 0)
  }
};
