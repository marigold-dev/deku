"use strict";
exports.__esModule = true;
var deku_js_interop_1 = require("deku_js_interop");
var grandma_cost = 5;
var factory_cost = 10;
var action_type;
(function (action_type) {
    action_type["increment_cookie"] = "increment_cookie";
    action_type["increment_grandma"] = "increment_grandma";
    action_type["increment_factory"] = "increment_factory";
})(action_type || (action_type = {}));
var print_message_with_source = function (message, source) {
    console.log(message);
    console.log(source);
};
var transition = function (_a) {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    var source = _a.source, op_hash = _a.op_hash, tx_hash = _a.tx_hash, operation = _a.operation;
    switch (operation) {
        case action_type.increment_cookie: {
            var source_value = JSON.parse((0, deku_js_interop_1.get)(source));
            print_message_with_source("Successfully parsed source:", source_value);
            console.log("Adding cookie");
            console.log(source_value);
            var cookie_baker = source_value.cookie_baker;
            //adding one cookie
            cookie_baker.number_of_cookie += 1;
            console.log("New cookies amount: " + cookie_baker.number_of_cookie);
            //update state
            print_message_with_source("successfully minted cookie for:", source_value);
            (0, deku_js_interop_1.set)(source, source_value);
            console.log("Saved state");
            break;
        }
        case action_type.increment_grandma: {
            var source_value = JSON.parse((0, deku_js_interop_1.get)(source));
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
                (0, deku_js_interop_1.set)(source, source_value);
                console.log("Saved state");
            }
            else {
                console.log("Not enough cookie to buy a grandma");
            }
            break;
        }
        case action_type.increment_factory: {
            var source_value = JSON.parse((0, deku_js_interop_1.get)(source));
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
                (0, deku_js_interop_1.set)(source, source_value);
                console.log("Saved state");
            }
            else {
                console.log("Not enough cookie to buy a factory");
            }
            break;
        }
    }
    //always need to do `JSON.parse(get(source));
};
(0, deku_js_interop_1.main)(
//tz address must be replaced by a correct one obtained with 
//deku-cli create-wallet
{
    "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc": {
        cookie_baker: { number_of_cookie: 0, number_of_grandma: 0, number_of_factory: 0 }
    }
}, transition);
