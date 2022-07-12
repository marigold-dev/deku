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

const transition = ( {source, op_hash, tx_hash, operation}) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any

    //always need to do `JSON.parse(get(source));`
    const source_value = JSON.parse(get(source));
    console.log("Successfully parsed source: " + source_value);
    console.log(source_value);
    console.log("adding cookie");
    //adding one cookie
    source_value.cookies += 1;
    console.log("new cookies amount: " + source_value.cookies);
    //update state
    set(source, JSON.stringify(source_value));
    console.log("successfully minted cookie for:" + source)
}

main(
    //tz address must be replaced by a correct one obtained with 
    //deku-cli create-wallet
    {"tz1LRHhTuoPn79STLaKbPpZUvB8QQk4WMeS1": 
        {cookies:0}
    }, transition)
