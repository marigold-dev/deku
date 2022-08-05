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
var saveState = function (source, sourceValue) {
    printMessageWithSource("Saving state", sourceValue);
    (0, deku_js_interop_1.set)(source, sourceValue);
    console.log("Successfully saved state");
};
var transition = function (tx) {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    var source = tx.source;
    var operation = tx.operation;
    console.log("Getting source");
    var sourceValue = JSON.parse((0, deku_js_interop_1.get)(source));
    var cookieBaker = (0, state_1.createCookieBaker)(sourceValue.cookieBaker.numberOfCookie, sourceValue.cookieBaker.numberOfCursor, sourceValue.cookieBaker.numberOfGrandma, sourceValue.cookieBaker.numberOfFarm, sourceValue.cookieBaker.numberOfMine, sourceValue.cookieBaker.numberOfFactory, sourceValue.cookieBaker.numberOfBank, sourceValue.cookieBaker.numberOfTemple, sourceValue.cookieBaker.numberOfWizard, sourceValue.cookieBaker.numberOfShipment, sourceValue.cookieBaker.numberOfAlchemy, sourceValue.cookieBaker.numberOfPortal, sourceValue.cookieBaker.numberOfTimeMachine, sourceValue.cookieBaker.numberOfAntimatter, sourceValue.cookieBaker.numberOfPrism, sourceValue.cookieBaker.numberOfChanceMaker, sourceValue.cookieBaker.numberOfFractal, sourceValue.cookieBaker.numberOfJavaScript, sourceValue.cookieBaker.numberOfIdleverse, sourceValue.cookieBaker.numberOfCordex, sourceValue.cookieBaker.numberOfFreeCursor, sourceValue.cookieBaker.numberOfFreeGrandma, sourceValue.cookieBaker.numberOfFreeFarm, sourceValue.cookieBaker.numberOfFreeMine, sourceValue.cookieBaker.numberOfFreeFactory, sourceValue.cookieBaker.numberOfFreeBank, sourceValue.cookieBaker.numberOfFreeTemple, sourceValue.cookieBaker.numberOfFreeWizard, sourceValue.cookieBaker.numberOfFreeShipment, sourceValue.cookieBaker.numberOfFreeAlchemy, sourceValue.cookieBaker.numberOfFreePortal, sourceValue.cookieBaker.numberOfFreeTimeMachine, sourceValue.cookieBaker.numberOfFreeAntimatter, sourceValue.cookieBaker.numberOfFreePrism, sourceValue.cookieBaker.numberOfFreeChanceMaker, sourceValue.cookieBaker.numberOfFreeFractal, sourceValue.cookieBaker.numberOfFreeJavaScript, sourceValue.cookieBaker.numberOfFreeIdleverse, sourceValue.cookieBaker.numberOfFreeCordex);
    switch (operation) {
        case actions_1.actions.incrementCookie: {
            var updatedCookieBaker = (0, state_1.addCookie)(cookieBaker);
            //update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted cookie");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementCursor: {
            var updatedCookieBaker = (0, state_1.addCursor)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted cursor");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementGrandma: {
            var updatedCookieBaker = (0, state_1.addGrandma)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted grandma");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementFarm: {
            var updatedCookieBaker = (0, state_1.addFarm)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted farm");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementMine: {
            var updatedCookieBaker = (0, state_1.addMine)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted mine");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementFactory: {
            var updatedCookieBaker = (0, state_1.addFactory)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted factory");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementBank: {
            var updatedCookieBaker = (0, state_1.addBank)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted bank");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementTemple: {
            var updatedCookieBaker = (0, state_1.addTemple)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted temple");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementWizard: {
            var updatedCookieBaker = (0, state_1.addWizard)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted wizard");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementShipment: {
            var updatedCookieBaker = (0, state_1.addShipment)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted shipment");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementAlchemy: {
            var updatedCookieBaker = (0, state_1.addAlchemy)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted alchemy");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementPortal: {
            var updatedCookieBaker = (0, state_1.addPortal)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted portal");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementTimeMachine: {
            var updatedCookieBaker = (0, state_1.addTimeMachine)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted time machine");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementAntimatter: {
            var updatedCookieBaker = (0, state_1.addAntimatter)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted antimatter");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementPrism: {
            var updatedCookieBaker = (0, state_1.addPrism)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted prism");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementChanceMaker: {
            var updatedCookieBaker = (0, state_1.addChanceMaker)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted chance maker");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementFractal: {
            var updatedCookieBaker = (0, state_1.addFractal)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted fractal");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementJavascript: {
            var updatedCookieBaker = (0, state_1.addJavascript)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted javascript");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementIdleverse: {
            var updatedCookieBaker = (0, state_1.addIdleverse)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted idleverse");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incrementCordex: {
            var updatedCookieBaker = (0, state_1.addCordex)(cookieBaker);
            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted cordex");
            saveState(source, sourceValue);
            break;
        }
    }
};
(0, deku_js_interop_1.main)(
//tz address must be replaced by a correct one obtained with 
//deku-cli create-wallet
{
    "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc": {
        cookieBaker: {
            numberOfCookie: 0,
            numberOfCursor: 0.,
            numberOfGrandma: 0.,
            numberOfFarm: 0.,
            numberOfMine: 0.,
            numberOfFactory: 0.,
            numberOfBank: 0.,
            numberOfTemple: 0.,
            numberOfWizard: 0.,
            numberOfShipment: 0.,
            numberOfAlchemy: 0.,
            numberOfPortal: 0.,
            numberOfTimeMachine: 0.,
            numberOfAntimatter: 0.,
            numberOfPrism: 0.,
            numberOfChanceMaker: 0.,
            numberOfFractal: 0.,
            numberOfJavaScript: 0.,
            numberOfIdleverse: 0.,
            numberOfCordex: 0.,
            numberOfFreeCursor: 0,
            numberOfFreeGrandma: 0,
            numberOfFreeFarm: 0,
            numberOfFreeMine: 0,
            numberOfFreeFactory: 0,
            numberOfFreeBank: 0,
            numberOfFreeTemple: 0,
            numberOfFreeWizard: 0,
            numberOfFreeShipment: 0,
            numberOfFreeAlchemy: 0,
            numberOfFreePortal: 0,
            numberOfFreeTimeMachine: 0,
            numberOfFreeAntimatter: 0,
            numberOfFreePrism: 0,
            numberOfFreeChanceMaker: 0,
            numberOfFreeFractal: 0,
            numberOfFreeJavaScript: 0,
            numberOfFreeIdleverse: 0,
            numberOfFreeCordex: 0,
            cursorCost: 0,
            grandmaCost: 0,
            farmCost: 0,
            mineCost: 0,
            factoryCost: 0,
            bankCost: 0,
            cursorCps: 0,
            grandmaCps: 0,
            farmCps: 0,
            mineCps: 0,
            factoryCps: 0,
            bankCps: 0,
            templeCps: 0,
            wizardCps: 0,
            shipmentCps: 0,
            alchemyCps: 0,
            portalCps: 0,
            timeMachineCps: 0,
            antimatterCps: 0,
            prismCps: 0,
            chanceMakerCps: 0,
            fractalCps: 0,
            javaScriptCps: 0,
            idleverseCps: 0,
            cordexCps: 0,
        }
    }
}, transition);
