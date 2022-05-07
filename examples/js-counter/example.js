const { main, set, get } = require("deku_js_interop");

const transition = (_sender, action_buffer) => {
  const action = JSON.parse(action_buffer.toString());
  let counter = get("counter");
  counter = Number.parseFloat(counter);

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

main(transition);