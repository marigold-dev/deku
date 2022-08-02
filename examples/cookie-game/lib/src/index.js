"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
var deku_js_interop_1 = require("deku_js_interop");
var state_1 = require("./state");
var actions_1 = require("./actions");
var printMessageWithSource = function (message, source) {
    console.log(message);
    console.log(source);
};
var saveState = function (source, source_value) {
    printMessageWithSource("Saving state", source_value);
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
    var cookieBaker = (0, state_1.createCookieBaker)(source_value.cookie_baker.numberOfCookie, source_value.cookie_baker.numberOfCursor, source_value.cookie_baker.numberOfGrandma, source_value.cookie_baker.numberOfFarm, source_value.cookie_baker.numberOfFreeCursor, source_value.cookie_baker.numberOfFreeGrandma, source_value.cookie_baker.numberOfFreeFarm);
    switch (operation) {
        case actions_1.actions.incrementCookie: {
            var updated_cookie_baker = (0, state_1.addCookie)(cookieBaker);
            //update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted cookie");
            saveState(source, source_value);
            break;
        }
        case actions_1.actions.incrementCursor: {
            var updated_cookie_baker = (0, state_1.addCursor)(cookieBaker);
            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted cursor");
            saveState(source, source_value);
            break;
        }
        case actions_1.actions.incrementGrandma: {
            var updated_cookie_baker = (0, state_1.addGrandma)(cookieBaker);
            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted grandma");
            saveState(source, source_value);
            break;
        }
        case actions_1.actions.incrementFarm: {
            var updated_cookie_baker = (0, state_1.addFarm)(cookieBaker);
            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted farm");
            saveState(source, source_value);
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
            numberOfCookie: 0,
            numberOfCursor: 0.,
            numberOfGrandma: 0.,
            numberOfFarm: 0.,
            numberOfFreeCursor: 0,
            numberOfFreeGrandma: 0,
            numberOfFreeFarm: 0,
            cursorCost: 0,
            grandmaCost: 0,
            farmCost: 0,
            cursorCps: 0,
            grandmaCps: 0,
            farmCps: 0
        }
    }
}, transition);
