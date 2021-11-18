type int_set is set (int)

const my_set : int_set = set [3; 2; 2; 1]

const contains_3 : bool = my_set contains 3

const set_size : nat = size (my_set)

const larger_set  : int_set = Set.add (4, my_set)

const smaller_set : int_set = Set.remove (3, my_set)

function update (var s : set (int)) : set (int) is block {
  patch s with set [4; 7]
} with s

const new_set : set (int) = update (my_set)

function sum (const acc : int; const i : int): int is acc + i

const sum_of_elements : int = Set.fold (sum, my_set, 0)

function loop (const s : set (int)) : int is block {
  var sum : int := 0;
  for element in set s block {
    sum := sum + element
  }
} with sum
