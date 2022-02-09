include (module type of Option);
module Let_syntax: {
  let some: 'a => option('a);
  let (let.none): (option('a), unit => option('a)) => option('a);
  let (let.some): (option('a), 'a => option('b)) => option('b);
  let (let.default): ('a, unit => option('a)) => 'a;
};
