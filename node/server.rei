let start: (~initial: State.t) => unit;
let get_state: unit => State.t;
let set_state: State.t => unit;
let get_port: unit => option(int);
