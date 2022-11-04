import React, { useState, useEffect } from "react";
import ReactDOM from "react-dom";

import { DekuToolkit } from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";

// The DekuClient uses Taquito for signing operations.
// Taquito supports a variety of hardware- and browser-based
// wallets, but for this demo we'll simply use a hard-coded private key
// and the InMemory signer.
const signer = new InMemorySigner(
  "edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR"
);

const deku = new DekuToolkit({
  signer,
  dekuRPC: "http://localhost:8080",
  tezosRPC: "http://localhost:20000",
});

const MyFirstDApp = () => {
  const [counter, setCounter] = useState(null);
  const [delta, setDelta] = useState(1);
  const handleInputChange = (event) => setDelta(event.target.value);
  const handleIncrement = () => deku.tx(JSON.stringify(["Increment", delta]));
  const handleDecrement = () => deku.tx(JSON.stringify(["Decrement", delta]));
  useEffect(() => {
    deku.subscribeToKey("counter", (counterState) =>
      setCounter(JSON.parse(counterState))
    );
  });
  return (
    <div>
      <p>
        <b>Current Counter State: {counter}</b>
      </p>
      <button onClick={handleIncrement}>-</button>
      <input value={delta} onChange={handleInputChange} />
      <button onClick={handleDecrement}>+</button>
    </div>
  );
};

ReactDOM.render(<MyFirstDApp />, document.getElementById("root"));
