// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"

const transition = (tx: transaction) => {
    console.log("Getting source");
    const currentValue = JSON.parse(get("state"));
    console.log("Current value: " + currentValue);
    const nextValue = tx.operation;
    console.log("New value: " + nextValue);
    set("state", nextValue);


}

main({ myState: "" }, transition);
