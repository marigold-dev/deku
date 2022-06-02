const { main, set, get } = require("deku_js_interop");

const transition = (sender, tx_hash, action_buffer) => {
  console.log(`sender: ${sender}, tx_hash: ${tx_hash}`)
  const action = JSON.parse(action_buffer.toString());
  let counter = get("counter");
  counter = Number.parseFloat(counter);
  console.log(`current counter: ${counter}`);

  switch (action.Action) {
    case "Increment": {
      set("counter", counter + 1);
      return;
    }
    case "Decrement": {
      set("counter", counter - 1);
      return;
    }
    default:
      throw new Error("Not known action")
  }
}

main({counter: 42}, transition);
