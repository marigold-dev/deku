import {main, get, set} from "deku_js_interop"

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
    number_of_cookies: number,
    number_of_grandmas: number,
    number_of_factories: number,
}

interface grandma {
    cookie_per_second : 2
}

interface factory {
    cookie_per_second : 10
}

enum actions {
    increment_cookie = "increment_cookie",
    increment_grandma = "increment_grandma",
    increment_factory = "increment_factory"
}

const print_message_with_source = (message, source) => {
    console.log(message);
    console.log(source);
}

const transition = ( {source, op_hash, tx_hash, operation}) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any

    switch (operation) {
        case actions.increment_cookie : {
            const source_value = JSON.parse(get(source));
            console.log("Successfully parsed source: ");
            console.log(source_value);
            console.log("adding cookie");
            //adding one cookie
            source_value.cookie_baker.number_of_cookies += 1;
            console.log("new cookies amount: " + source_value.number_of_cookies);
            //update state
            set(source, JSON.stringify(source_value));
            print_message_with_source("successfully minted cookie for:", source_value);
            break;
        }
        case actions.increment_grandma : {
            const source_value = JSON.parse(get(source));
            console.log("Successfully parsed source: ");
            console.log(source_value);
            console.log("adding grandma");
            //adding one cookie
            source_value.cookie_baker.number_of_grandmas += 1;
            console.log("new grandmas amount: " + source_value.number_of_grandmas);
            //update state
            set(source, JSON.stringify(source_value));
            print_message_with_source("successfully minted grandma for:", source_value);
            break;
        }
        case actions.increment_factory : {
            const source_value = JSON.parse(get(source));
            console.log("Successfully parsed source: ");
            console.log(source_value);
            console.log("adding factory");
            //adding one factory
            source_value.factory_baker.number_of_factories += 1;
            console.log("new factories amount: " + source_value.number_of_factories);
            //update state
            set(source, JSON.stringify(source_value));
            print_message_with_source("successfully minted factory for:", source_value);
            break;
        }
    }
    //always need to do `JSON.parse(get(source));
    
}

main(
    //tz address must be replaced by a correct one obtained with 
    //deku-cli create-wallet
    {"tz1LRHhTuoPn79STLaKbPpZUvB8QQk4WMeS1": 
        {cookie_baker:
            { number_of_cookies: 0, number_of_grandmas:0, number_of_factories:0}}
    }, transition)
