"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
var deku_js_interop_1 = require("deku_js_interop");
var state_1 = require("./state");
var actions_1 = require("./actions");
var print_message_with_source = function (message, source) {
    console.log(message);
    console.log(source);
};
var save_state = function (source, source_value) {
    print_message_with_source("Saving state", source_value);
    (0, deku_js_interop_1.set)(source, source_value);
    console.log("Successfully saved state");
};
var transition = function (tx) {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    var source = tx.source;
    var operation = tx.operation;
    console.log("Getting source");
    var source_value = JSON.parse((0, deku_js_interop_1.get)(source));
    var cookie_baker = (0, state_1.create_cookie_baker)(source_value.cookie_baker.number_of_cookie, source_value.cookie_baker.number_of_cursor, source_value.cookie_baker.number_of_grandma, source_value.cookie_baker.number_of_farm, source_value.cookie_baker.number_of_free_cursor, source_value.cookie_baker.number_of_free_grandma, source_value.cookie_baker.number_of_free_farm);
    switch (operation) {
        case actions_1.action_type.increment_cookie: {
            var updated_cookie_baker = (0, state_1.add_cookie)(cookie_baker);
            //update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted cookie");
            save_state(source, source_value);
            break;
        }
        case actions_1.action_type.increment_cursor: {
            var updated_cookie_baker = (0, state_1.add_cursor)(cookie_baker);
            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted cursor");
            save_state(source, source_value);
            break;
        }
        case actions_1.action_type.increment_grandma: {
            var updated_cookie_baker = (0, state_1.add_grandma)(cookie_baker);
            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted grandma");
            save_state(source, source_value);
            break;
        }
        case actions_1.action_type.increment_farm: {
            var updated_cookie_baker = (0, state_1.add_farm)(cookie_baker);
            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted farm");
            save_state(source, source_value);
            break;
        }
    }
};
(0, deku_js_interop_1.main)(
//tz address must be replaced by a correct one obtained with 
//deku-cli create-wallet
{
    "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc": {
        cookie_baker: {
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
}, transition);
