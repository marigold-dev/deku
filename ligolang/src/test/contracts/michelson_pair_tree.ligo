type inner_storage is michelson_pair(int,"one",nat,"two")
type storage is michelson_pair (string,"three",inner_storage,"four")

type return is list(operation) * storage

function main (const action : unit; const store : storage) : return is block {
  const foo : storage = ("foo",(1,2n)) ;
} with ((nil : list(operation)), (foo: storage))