// Test set iteration

let aggregate ( (i,j) : int list * int) : int list = j :: i

let fold_op (s : int set) : int list = Set.fold aggregate s ([] : int list)

let aggregate ( (i,j) : int * int list) : int list = i :: j
let fold_right (s : int set ) : int list = Set.fold_desc aggregate  s ([] : int list)