type inner_storage is michelson_or(int,"one",nat,"two")
type storage is michelson_or (int,"three",inner_storage,"four")

type return is list(operation) * storage

function main (const action : unit; const store : storage) : return is block {
  const foo : storage = (M_right ((M_left(1) : inner_storage)) : storage) ;
} with ((nil : list(operation)), (foo: storage))
