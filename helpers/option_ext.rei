include (module type of Option);
module Let_syntax: {
  let some: 'a => t('a);
  let (let.none): (t('a), unit => t('a)) => t('a);
  let (let.some): (t('a), 'a => t('b)) => t('b);
  let (let.default): ('a, unit => t('a)) => 'a;
};
module Syntax: {
  let ( let* ): (t('a), 'a => t('b)) => t('b);
  let ( and* ): (t('a), t('b)) => t(('a, 'b));
  let (let+): ('a => 'b, t('a)) => t('b);
  let (and+): (t('a), t('b)) => t(('a, 'b));
};
