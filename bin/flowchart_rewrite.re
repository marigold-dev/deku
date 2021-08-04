// In the actual code, these would be replaced with real types rather than unit
type signature = unit;
type node = unit;
type block = unit;

// fake monad for illustration purposes
// we'll need some state on top of the finite state machine - that could/should be stored in a state monad
// plus we can put any effects we need in there too
type monad('a) = Pure('a)

module Received_signature {
  type initial_state = [`Received_signature(signature)]
  let start: signature => monad(initial_state) = (sig) => Pure(`Received_signature(sig)); 

  type state = [initial_state | `Valid(signature) | `Unknown(signature) | `Signed(signature) | `Is_present(block) | `Should_request(signature)]

  let is_valid: initial_state => monad([`Valid(signature) | `Done]) = assert false
  let is_known: [`Valid(signature)] => monad([`Known(signature) | `Done]) = assert false
  let is_known: [`Known(signature)] => monad([`Signed(signature) | `Done]) = assert false

  let is_present: [`Signed(signature)] => monad([`Is_present(block) | `Should_request(signature)]) = assert false

  let should_request: [`Should_request(signature)] => monad([`Done]) = assert(false)

  // This function needs no logic, only exists to make the flowchart simpler
  let when_present: [`Is_present(block)] => monad([`Should_add_to_pool(block)]) = assert(false); 
}


module Received_block {
  type initial_state = [`Received_block(block)]
  let start: block => monad(initial_state) = (block) => Pure(`Received_block(block)); 
  type state = [initial_state | `Valid(block) | `Unknown(block) | `Should_add_to_pool(block)]; 

  let is_valid: initial_state => monad([`Valid(block) | `Done]) = assert(false);
  let is_unknown: [`Valid(block)] => monad([`Unknown(block) | `Done]) = assert(false);

  // This function needs no logic, only exists to make the flowchart simpler
  let when_unknown: [`Valid(block)] => monad([`Should_add_to_pool(block)]) = assert(false);
}


// ... etc. for all other parts of the flowchart, including definitions for what to do when encountering `Should_add_to_pool(block) etc.


type state = [Received_signature.state | Received_block.state];


let advance: state => monad(state) = (state) => switch (state) {
  // in here is a giant bit of code that advances the state machine one step, by calling the functions defined above. 
  // it could be automatically generated in the future but writing it by hand wouldn't be so bad
};

// this just repeatedly calls advance until a function returns `Done
let apply: state => monad([`Done]) = (state) => assert (false)

