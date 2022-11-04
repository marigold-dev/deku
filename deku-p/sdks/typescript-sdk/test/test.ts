import { main, get, set } from "../src/interop";

const initialState = { counter: 1 };

const transition = ({ source, operation, tickets }) => {
  console.log("receive a transaction");
  console.log(source);
  console.log(operation);
  console.log(tickets);
  console.log("counter:");
  const counter = JSON.parse(get("counter")!);
  console.log(counter);
  const next_counter = counter + 1;
  set("counter", JSON.stringify(next_counter));

  console.log("OK OK OK OK OK");
};

main(initialState, transition);
