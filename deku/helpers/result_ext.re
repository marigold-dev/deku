include Result;

module Let_syntax = {
  let ok = Result.ok;
  let (let.ok) = Result.bind;
  let (let.assert) = ((message, bool), f) => bool ? f() : Error(message);
};
