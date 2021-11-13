type dup ('a) = ('a, 'a);

let diag : (_a => dup (_a)) = (x : _a) => (x, x);

let rec rev  : ((list (_a), list (_a)) => list (_a)) = ((xs, acc) : (list (_a), list (_a))) : list (_a) =>
  switch xs {
  | [] => acc
  | [x,... xs] => rev (xs, [x,... acc])
  };

let rev : (list (_a) => list (_a)) = (xs : list (_a)) : list (_a) => rev (xs, ([] : list (_a)));

let rec zip : ((list (_a), list (_b), list((_a, _b))) => list ((_a, _b))) = ((xs, ys, acc) : (list (_a), list (_b), list ((_a, _b)))) : list ((_a, _b)) =>
  switch (xs) {
  | [] =>
       switch (ys) {
       | [] => acc
       | _  => failwith ("oops")
       }
  | [x, ...xs] =>
        switch (ys) {
        | [] => failwith ("oops")
        | [y, ...ys] => zip (xs, ys, [(x, y), ...acc])
        }
  };

let zip : (list (_a) => list (_b) => list ((_a, _b))) = (xs : list (_a)) => (ys : list (_b)) : list ((_a, _b)) => rev (zip (xs, ys, ([] : list ((_a, _b)))));

let self_zip : (list (_tau) => list ((_tau, _tau))) = (xs : list (_tau)) : list ((_tau, _tau)) =>
  let (xs, ys) = diag (xs);
  (zip (xs))(ys);

let v : list ((string, string)) = self_zip (["a","b"]);
let w : list ((int, nat)) = (zip ([1,2,3]))([4n,5n,6n]);
