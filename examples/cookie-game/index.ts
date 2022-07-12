import { main, get, set } from "deku_js_interop"

// const fct = ({param1, param2}: custom_type) => {
//     console.log("Here I am");
//     console.log(param1);
//     console.log(param2);
// }

// interface custom_type {
//     param1: string,
//     param2: number
// }

// fct({param1: "salut", param2:3});

interface cookie_baker {
    number_of_cookie: number,
    number_of_grandma: number,
    number_of_factory: number,
}

const grandma_cost = 5;
const factory_cost = 10;

interface grandma {
    cookie_per_second: 2
}

interface factory {
    cookie_per_second: 10
}

enum action_type {
    increment_cookie = "increment_cookie",
    increment_grandma = "increment_grandma",
    increment_factory = "increment_factory"
}

const print_message_with_source = (message, source) => {
    console.log(message);
    console.log(source);
}

const transition = ({ source, op_hash, tx_hash, operation }) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any

    switch (operation) {
        case action_type.increment_cookie: {
            const source_value = JSON.parse(get(source));
            print_message_with_source("Successfully parsed source:", source_value);
            console.log("Adding cookie");
            console.log(source_value);
            const cookie_baker = source_value.cookie_baker;
            //adding one cookie
            cookie_baker.number_of_cookie += 1;
            console.log("New cookies amount: " + cookie_baker.number_of_cookie);
            //update state
            print_message_with_source("successfully minted cookie for:", source_value);
            set(source, source_value);
            console.log("Saved state");
            break;
        }
        case action_type.increment_grandma: {
            const source_value = JSON.parse(get(source));
            print_message_with_source("Successfully parsed source:", source_value);
            if (source_value.cookie_baker.number_of_cookie >= grandma_cost) {
                console.log("Adding grandma");
                //adding one cookie
                source_value.cookie_baker.number_of_grandma += 1;
                source_value.cookie_baker.number_of_cookie -= grandma_cost;
                console.log("New grandmas amount: " + source_value.cookie_baker.number_of_grandma);
                console.log("New cookies amount: " + source_value.cookie_baker.number_of_cookie);

                //update state
                print_message_with_source("successfully minted grandma for:", source_value);
                set(source, source_value);
                console.log("Saved state");
            } else {
                console.log("Not enough cookie to buy a grandma");
            }
            break;
        }
        case action_type.increment_factory: {
            const source_value = JSON.parse(get(source));
            if (source_value.cookie_baker.number_of_cookie >= factory_cost) {
                print_message_with_source("Successfully parsed source:", source_value);
                console.log("Adding factory");
                //adding one factory
                source_value.cookie_baker.number_of_factory += 1;
                source_value.cookie_baker.number_of_cookie -= factory_cost;
                console.log("New factories amount: " + source_value.cookie_baker.number_of_factory);
                console.log("New cookies amount: " + source_value.cookie_baker.number_of_cookie);
                //update state
                print_message_with_source("successfully minted factory for:", source_value);
                set(source, source_value);
                console.log("Saved state");
            } else {
                console.log("Not enough cookie to buy a factory");
            }
            break;
        }
    }
    //always need to do `JSON.parse(get(source));

}

main(
    //tz address must be replaced by a correct one obtained with 
    //deku-cli create-wallet
    {
        "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc":
        {
            cookie_baker:
                { number_of_cookie: 0, number_of_grandma: 0, number_of_factory: 0 }
        }
    }, transition)
