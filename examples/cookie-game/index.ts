// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"

interface cookie_baker_state {
    /* minted by user */
    number_of_cookie: number,
    number_of_cursor: number,
    number_of_grandma: number,
    number_of_farm: number,

    /* Gift from application */
    /* TODO: add the rule to generate them! */
    number_of_free_cursor: number,
    number_of_free_grandma: number,
    number_of_free_farm: number,

    cursor_cost: number;
    grandma_cost: number,
    farm_cost: number;

    /* Cookie per second*/
    cursor_cps: number;
    grandma_cps: number;
    farm_cps: number;

    total_cps: number;
}

const cursor_initial_cps = 0.1;
const grandma_initial_cps = 1;
const farm_initial_cps = 8;

const initial_cursor_cost = 15;
const initial_grandma_cost = 100;
const initial_farm_cost = 1100;

const calculate_cost = (action: action_type, state: cookie_baker_state) => {
    console.log("Calculating price for next building");
    let price2 = 0;
    switch (action) {
        case action_type.increment_cursor:
            const cursor_cost = 1.15 * initial_cursor_cost;
            price2 = Math.pow(cursor_cost, state.number_of_cursor - state.number_of_free_cursor);
            break;
        case action_type.increment_grandma:
            const grandma_cost = 1.15 * initial_grandma_cost;
            price2 = Math.pow(grandma_cost, state.number_of_grandma - state.number_of_free_grandma);
            break;
        case action_type.increment_farm:
            const farm_cost = 1.15 * initial_farm_cost;
            price2 = Math.pow(farm_cost, state.number_of_farm - state.number_of_free_farm);
            break;
    }
    return Math.floor(price2);
}

enum action_type {
    increment_cookie = "cookie",
    increment_cursor = "cursor",
    increment_grandma = "grandma",
    increment_farm = "farm"
}
// @ts-ignore
const print_message_with_source = (message: string, source) => {
    console.log(message);
    console.log(source);
}
const save_state = (source: transaction, source_value: JSON) => {
    console.log("Saving state");
    set(source, source_value);
    console.log("Successfully saved state");
}

const transition = (tx: transaction) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    const source = tx.source;
    const operation = tx.operation;
    console.log("Getting source");
    const source_value = JSON.parse(get(source));
    print_message_with_source("The cookie that you have now is:", source_value);

    switch (operation) {
        case action_type.increment_cookie: {
            print_message_with_source("Adding cookie", source_value);
            //adding one cookie
            source_value.cookie_baker_state.number_of_cookie += 1;
            console.log("New cookies amount: " + source_value.cookie_baker_state.number_of_cookie);
            //update state
            print_message_with_source("The cookie that you have now is:", source_value);
            save_state(source, source_value);
            break;
        }
        case action_type.increment_cursor: {
            if (source_value.cookie_baker_state.number_of_cookie >= source_value.cookie_baker_state.cursor_cost) {
                console.log("Adding cursor");
                //adding one cursor
                source_value.cookie_baker_state.number_of_cursor += 1;
                console.log("New cursors amount: " + source_value.cookie_baker_state.number_of_cursor);
                // cookie = cookie - cursor_cost
                source_value.cookie_baker_state.number_of_cookie -= initial_cursor_cost;
                console.log("New cookies amount: " + source_value.cookie_baker_state.number_of_cookie);

                //update state
                print_message_with_source("Successfully minted cursor for:", source_value);

                source_value.cookie_baker_state.cursor_cost = calculate_cost(action_type.increment_cursor, source_value.cookie_baker_state)
                console.log("New cursor cost: " + source_value.cookie_baker_state.cursor_cost)
                save_state(source, source_value);

            } else {
                console.log("Not enough cookie to buy a cursor. The cookie you need have to be " + initial_cursor_cost);
            }
            break;
        }
        case action_type.increment_grandma: {
            if (source_value.cookie_baker_state.number_of_cookie >= initial_grandma_cost) {
                console.log("Adding grandma");
                //adding one grandma
                source_value.cookie_baker_state.number_of_grandma += 1;
                console.log("New grandmas amount: " + source_value.cookie_baker_state.number_of_grandma);
                // cookie = cookie - grandma_cost
                source_value.cookie_baker_state.number_of_cookie -= initial_grandma_cost;
                console.log("New cookies amount: " + source_value.cookie_baker_state.number_of_cookie);

                //update state
                print_message_with_source("Successfully minted grandma for:", source_value);
                save_state(source, source_value);
            } else {
                console.log("Not enough cookie to buy a grandma. The cookie you need have to be " + initial_grandma_cost);
            }
            break;
        }
        case action_type.increment_farm: {
            if (source_value.cookie_baker_state.number_of_cookie >= initial_farm_cost) {
                console.log("Adding farm");
                //adding one farm
                source_value.cookie_baker_state.number_of_farm += 1;
                console.log("New farms amount: " + source_value.cookie_baker_state.number_of_farm);
                // cookie = cookie - farm_cost
                source_value.cookie_baker_state.number_of_cookie -= initial_farm_cost;
                console.log("New cookies amount: " + source_value.cookie_baker_state.number_of_cookie);

                //update state
                print_message_with_source("Successfully minted farm for:", source_value);
                save_state(source, source_value);
            } else {
                console.log("Not enough cookie to buy a farm. The cookie you need have to be " + initial_farm_cost);
            }
            break;
        }
    }
}

main(
    //tz address must be replaced by a correct one obtained with 
    //deku-cli create-wallet
    {
        "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc":
        {
            cookie_baker_state:
            {
                number_of_cookie: 0,
                number_of_cursor: 0.,
                number_of_grandma: 0.,
                number_of_farm: 0.,
                number_of_free_cursor: 0,
                number_of_free_grandma: 0,
                number_of_free_farm: 0,
                cursor_cost: initial_cursor_cost,
                grandma_cost: initial_grandma_cost,
                farm_cost: initial_farm_cost,
                cursor_cps: 0,
                grandma_cps: 0,
                farm_cps: 0,
                total_cps: 0
            }
        }
    }, transition)
