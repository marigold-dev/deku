const x = block {
  const l = list [1; 2; 3;]
  } with case l of [
  | list [_; _; _;] -> 0
  | _               -> 1
  ];
