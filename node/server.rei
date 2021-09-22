let start: (~initial: Node_state.t) => unit;
let get_state: unit => Node_state.t;
let set_state: Node_state.t => unit;
let get_port: unit => option(int);
