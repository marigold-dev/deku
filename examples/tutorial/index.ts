// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"

const transition = (tx: transaction) => {
    console.log("Getting source");
    let source_value = JSON.parse(get("my_state"));
    console.log("Current value: " + source_value);
    // tx.operation is the last argument of `deku-cli` command line
    source_value = tx.operation;
    console.log("New value: " + source_value);
    set("my_state", source_value);
}

main({ my_state: "" }, transition);