type parameter = Add(int) | Subtract(int);

type storage = int;

let main = ((p, s): (parameter, storage)) => {
  switch(p){
  | Add n => ([] : list(operation), s + n)
  | Subtract n => ([] : list(operation), s - n)
  }
};
