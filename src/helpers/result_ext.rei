include (module type of {
  include Result;
});

module Let_syntax: {
  let ok: 'a => result('a, 'b);
  let (let.ok): (result('a, 'b), 'a => result('c, 'b)) => result('c, 'b);
  let (let.assert): (('a, bool), unit => result('b, 'a)) => result('b, 'a);
};
