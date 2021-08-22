include (module type of List);

let find_index: ('a => bool, t('a)) => option(int);
let in_order_uniq: (('a, 'a) => int, t('a)) => t('a);

let fold_left_ok:
  (('a, 'b) => result('a, 'c), 'a, list('b)) => result('a, 'c);
