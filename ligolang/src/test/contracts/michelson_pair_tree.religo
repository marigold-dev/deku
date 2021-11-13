type inner_storage = michelson_pair(int,"one",nat,"two");
type storage = michelson_pair(int,"three",inner_storage,"four");

type return = (list (operation) , storage);

let main = ((action, store) : (unit , storage)) : return => {
  let foo = (3,(1,2n)) ;
  (([] : list(operation)), (foo: storage))
};