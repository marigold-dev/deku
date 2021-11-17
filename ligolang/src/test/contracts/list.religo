type storage = (int, list (int));

type parameter = list (int);

type return = (list (operation), storage);

let x : list (int) = [];
let y : list (int) = [3, 4, 5];
let z : list (int) = [2, ...y];

let main = ((action, s) : (parameter, storage)) : return => {
  let storage =
    switch (action) {
    | [] => s
    | [hd, ...tl] => (s[0] + hd, tl)
    };
  ([]: list(operation), storage);
};

let size_ = (s : list (int)) : nat => List.length (s);

let fold_op = (s : list (int)) : int => {
  let aggregate = (t: (int, int)) => t[0] + t[1];
  List.fold (aggregate, s, 10);
};

let map_op = (s : list (int)) : list (int) =>
  List.map ((cur : int) => cur + 1, s);

let iter_op = (s : list (int)) : unit => {
  let do_nothing = (_useless : int):unit => unit;
  List.iter (do_nothing, s);
};
