const { main, get, set } = require("@marigold-dev/deku-p-sdk");

const transition = (tx) => {
  // Parse the operation data
  const operation = JSON.parse(tx.operation);
  console.log("Parsed operation:", operation);
  const [operationKind, value] = operation;

  // Retrieve the current state of the 'counter' key.
  // This is an in-memory lookup - no IO required.
  const counter = JSON.parse(get("counter"));
  console.log("Current state:", counter);

  switch (operation[0]) {
    case "Increment":
      set("counter", JSON.stringify(counter + value));
      break;
    case "Decrement":
      set("counter", JSON.stringify(counter - value));
    default:
      const error = `Unrecognized operation kind: ${operationKind}`;
      console.error(error);
      // Signal an error to the user by returning a string
      return error;
  }
};

// Here we define the initial state of blockchain's key-value
// store on the genesis block.
const initialState = {
  counter: JSON.stringify(42),
};

main(initialState, transition);
