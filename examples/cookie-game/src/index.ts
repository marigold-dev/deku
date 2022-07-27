// @ts-ignore
import { main, get, set } from "deku_js_interop"
import { Cookie_baker } from "./state"
import { action_type } from "./actions"

const print_message_with_source = (message: string, source: transaction) => {
    console.log(message);
    console.log(source);
}

const save_state = (source: transaction, source_value: Cookie_baker) => {
    print_message_with_source("Saving state", source_value);
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
    const cookie_baker =
        new Cookie_baker(
            source_value.cookie_baker.number_of_cookie,
            source_value.cookie_baker.number_of_cursor,
            source_value.cookie_baker.number_of_grandma,
            source_value.cookie_baker.number_of_farm,
            source_value.cookie_baker.number_of_free_cursor,
            source_value.cookie_baker.number_of_free_grandma,
            source_value.cookie_baker.number_of_free_farm
        );


    switch (operation) {
        case action_type.increment_cookie: {
            cookie_baker.add_cookie();
            //update state
            source_value.cookie_baker = cookie_baker;
            console.log("Successfully minted cookie");
            save_state(source, source_value);
            break;
        }
        case action_type.increment_cursor: {
            cookie_baker.add_cursor();

            //action successful, update state
            source_value.cookie_baker = cookie_baker;
            console.log("Successfully minted cursor");
            save_state(source, source_value);
            break;
        }
        case action_type.increment_grandma: {
            cookie_baker.add_grandma();

            //action successful, update state
            source_value.cookie_baker = cookie_baker;
            console.log("Successfully minted grandma");
            save_state(source, source_value);
            break;
        }
        case action_type.increment_farm: {
            cookie_baker.add_farm();

            //action successful, update state
            source_value.cookie_baker = cookie_baker;
            console.log("Successfully minted farm");
            save_state(source, source_value);
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
            cookie_baker:
            {
                number_of_cookie: 0,
                number_of_cursor: 0.,
                number_of_grandma: 0.,
                number_of_farm: 0.,
                number_of_free_cursor: 0,
                number_of_free_grandma: 0,
                number_of_free_farm: 0,
                cursor_cost: 0,
                grandma_cost: 0,
                farm_cost: 0,
                cursor_cps: 0,
                grandma_cps: 0,
                farm_cps: 0
            }
        }
    }, transition)
