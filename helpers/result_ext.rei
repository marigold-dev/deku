include (module type of {
  include Result;
});

module Let_syntax: {
  let ok: 'a => t('a, 'b);
  let (let.ok): (t('a, 'b), 'a => t('c, 'b)) => t('c, 'b);
  let (let.assert): (('a, bool), unit => t('b, 'a)) => t('b, 'a);
};
module Syntax: {
  let ( let* ): (t('a, 'b), 'a => t('c, 'b)) => t('c, 'b);
  let ( and* ): (t('a, 'b), t('c, 'b)) => t(('a, 'c), 'b);
  let (let+): ('a => 'b, t('a, 'c)) => t('b, 'c);
  let (and+): (t('a, 'b), t('c, 'b)) => t(('a, 'c), 'b);
};
