// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"

interface cookie_baker {
    number_of_cookie: number,
    number_of_grandma: number,
    number_of_factory: number,
}

const cursor_cost = 3;
const grandma_cost = 10;
const factory_cost = 20;

interface cursor {
    cookie_per_second: 0.5
}

interface grandma {
    cookie_per_second: 1
}

interface factory {
    cookie_per_second: 5
}

enum action_type {
    increment_cookie = "cookie",
    increment_cursor = "cursor",
    increment_grandma = "grandma",
    increment_factory = "factory"
}
// @ts-ignore
const print_message_with_source = (message:string, source) => {
    console.log(message);
    console.log(source);
}
const save_state = (source: transaction, source_value:JSON) => {
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
            source_value.cookie_baker.number_of_cookie += 1;
            console.log("New cookies amount: " + source_value.cookie_baker.number_of_cookie);
            //update state
            print_message_with_source("The cookie that you have now is:", source_value);
            save_state(source, source_value);
            break;
        }
        case action_type.increment_cursor: {
            if (source_value.cookie_baker.number_of_cookie >= cursor_cost) {
                console.log("Adding cursor");
                //adding one cursor
                source_value.cookie_baker.number_of_cursor += 1;
                console.log("New cursors amount: " + source_value.cookie_baker.number_of_cursor);
                // cookie = cookie - cursor_cost
                source_value.cookie_baker.number_of_cookie -= cursor_cost;
                console.log("New cookies amount: " + source_value.cookie_baker.number_of_cookie);

                //update state
                print_message_with_source("Successfully minted cursor for:", source_value);
                save_state(tx, source_value);
            } else {
                console.log("Not enough cookie to buy a cursor. The cookie you need have to be " + cursor_cost);
            }
            break;
        }
        case action_type.increment_grandma: {
            if (source_value.cookie_baker.number_of_cookie >= grandma_cost) {
                console.log("Adding grandma");
                //adding one grandma
                source_value.cookie_baker.number_of_grandma += 1;
                console.log("New grandmas amount: " + source_value.cookie_baker.number_of_grandma);
                // cookie = cookie - grandma_cost
                source_value.cookie_baker.number_of_cookie -= grandma_cost;
                console.log("New cookies amount: " + source_value.cookie_baker.number_of_cookie);

                //update state
                print_message_with_source("Successfully minted grandma for:", source_value);
                save_state(tx, source_value);
            } else {
                console.log("Not enough cookie to buy a grandma. The cookie you need have to be " + grandma_cost);
            }
            break;
        }
        case action_type.increment_factory: {
            if (source_value.cookie_baker.number_of_cookie >= factory_cost) {
                console.log("Adding factory");
                //adding one factory
                source_value.cookie_baker.number_of_factory += 1;
                console.log("New factories amount: " + source_value.cookie_baker.number_of_factory);
                // cookie = cookie - factory_cost
                source_value.cookie_baker.number_of_cookie -= factory_cost;
                console.log("New cookies amount: " + source_value.cookie_baker.number_of_cookie);
                
                //update state
                print_message_with_source("Successfully minted factory for:", source_value);
                save_state(tx, source_value);
            } else {
                console.log("Not enough cookie to buy a factory. The cookie you need have to be " + factory_cost);
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
            cookie_baker:
                { number_of_cookie: 0, number_of_cursor: 0, number_of_grandma: 0, number_of_factory: 0 }
        }
    }, transition)
