type parameter = Compute((int => int)) | Set(int);

type storage = int;

let main = ((p, s): (parameter, storage)) => 
  switch(p){
  | Compute func => ([] : list(operation), func(s))
  | Set n => ([] : list(operation), n)
  };
