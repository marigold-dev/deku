import { main, set, get } from "deku_js_interop";

enum ActionType {
    Increment = "Increment",
    Decrement = "Decrement"
}

interface Action {
    type: ActionType,
    payload: {
        sender: string,
        tx_hash: string,
    }
}

/**
 * A pure function that returns a new state
 * @param state the state to update
 * @param action see the Action interface
 * @returns the next state
 */
const apply_transition = (state: number, action: Action) => {
    switch (action.type) {
        case ActionType.Increment:
            return state + 1;
        case ActionType.Decrement:
            return state - 1;
        default:
            return state;
    }
}

/**
 * 
 * @param sender the address of the sender of this operation
 * @param tx_hash the hash of the transacion
 * @param action_buffer the custom payload of the transaction, in this example it's the string '"Increment"' or '"Decrement"'
 * @returns 
 */
const transition = (sender, tx_hash, action_buffer) => {
    console.log(`sender: ${sender}, tx_hash: ${tx_hash}`)
    const action_type = JSON.parse(action_buffer.toString());
    const raw_counter = get("counter"); // Get the counter from the state, it returns a Buffer
    const action = {
        type: action_type,
        payload: {
            sender,
            tx_hash
        }
    }
    const current_counter = parseInt(raw_counter.toString());
    const next_counter = apply_transition(current_counter, action);
    set("counter", next_counter);
    console.log(`next counter value: ${next_counter}`);
    return;
}

// {counter: 42} is the initial state, we are defining an entry in the deku state with the key counter and value 42
// You can access this value with get("counter")
// You can set this value with set("counter", 24)
main({ counter: 42 }, transition);