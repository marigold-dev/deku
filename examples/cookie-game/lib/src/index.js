"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
var deku_js_interop_1 = require("deku_js_interop");
var state_1 = require("./state");
var actions_1 = require("./actions");
var saveState = function (source, sourceValue) {
    (0, deku_js_interop_1.set)(source, sourceValue);
    console.log("Successfully saved state: " + JSON.stringify(sourceValue));
};
var transition = function (tx) {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    var source = tx.source;
    var operation = tx.operation;
    console.log("source: " + source);
    var sourceValue = JSON.parse((0, deku_js_interop_1.get)(source));
    console.log("sourceValue: " + sourceValue);
    var cookieBaker;
    if (sourceValue === undefined || sourceValue === null) {
        sourceValue = { source: source };
        console.log("sourceValue: " + sourceValue);
        cookieBaker = (0, state_1.createCookieBaker)(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    }
    else {
        cookieBaker = (0, state_1.createCookieBaker)(sourceValue.cookieBaker.cookies, sourceValue.cookieBaker.cursors, sourceValue.cookieBaker.grandmas, sourceValue.cookieBaker.farms, sourceValue.cookieBaker.mines, sourceValue.cookieBaker.factories, sourceValue.cookieBaker.banks, sourceValue.cookieBaker.temples, sourceValue.cookieBaker.wizards, sourceValue.cookieBaker.shipments, sourceValue.cookieBaker.alchemies, sourceValue.cookieBaker.portals, sourceValue.cookieBaker.timeMachines, sourceValue.cookieBaker.antimatters, sourceValue.cookieBaker.prisms, sourceValue.cookieBaker.chanceMakers, sourceValue.cookieBaker.fractals, sourceValue.cookieBaker.javaScripts, sourceValue.cookieBaker.idleverses, sourceValue.cookieBaker.cordexs, sourceValue.cookieBaker.freeCursor, sourceValue.cookieBaker.freeGrandma, sourceValue.cookieBaker.freeFarm, sourceValue.cookieBaker.freeMine, sourceValue.cookieBaker.freeFactory, sourceValue.cookieBaker.freeBank, sourceValue.cookieBaker.freeTemple, sourceValue.cookieBaker.freeWizard, sourceValue.cookieBaker.freeShipment, sourceValue.cookieBaker.freeAlchemy, sourceValue.cookieBaker.freePortal, sourceValue.cookieBaker.freeTimeMachine, sourceValue.cookieBaker.freeAntimatter, sourceValue.cookieBaker.freePrism, sourceValue.cookieBaker.freeChanceMaker, sourceValue.cookieBaker.freeFractal, sourceValue.cookieBaker.FreeJavaScript, sourceValue.cookieBaker.freeIdleverse, sourceValue.cookieBaker.freeCordex);
    }
    sourceValue.cookieBaker = cookieBaker;
    console.log("sourceValue.cookieBaker: " + JSON.stringify(sourceValue.cookieBaker));
    switch (operation) {
        case actions_1.actions.incr_Cookie: {
            //update state
            sourceValue.cookieBaker = (0, state_1.addCookie)(cookieBaker);
            console.log("Successfully minted cookie");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Cursor: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addCursor)(cookieBaker);
            console.log("Successfully minted cursor");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Grandma: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addGrandma)(cookieBaker);
            console.log("Successfully minted grandma");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Farm: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addFarm)(cookieBaker);
            console.log("Successfully minted farm");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Mine: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addMine)(cookieBaker);
            console.log("Successfully minted mine");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Factory: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addFactory)(cookieBaker);
            console.log("Successfully minted factory");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Bank: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addBank)(cookieBaker);
            console.log("Successfully minted bank");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Temple: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addTemple)(cookieBaker);
            console.log("Successfully minted temple");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Wizard: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addWizard)(cookieBaker);
            console.log("Successfully minted wizard");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Shipment: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addShipment)(cookieBaker);
            console.log("Successfully minted shipment");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Alchemy: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addAlchemy)(cookieBaker);
            console.log("Successfully minted alchemy");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Portal: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addPortal)(cookieBaker);
            console.log("Successfully minted portal");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_TimeMachine: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addTimeMachine)(cookieBaker);
            console.log("Successfully minted time machine");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Antimatter: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addAntimatter)(cookieBaker);
            console.log("Successfully minted antimatter");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Prism: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addPrism)(cookieBaker);
            console.log("Successfully minted prism");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_ChanceMaker: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addChanceMaker)(cookieBaker);
            console.log("Successfully minted chance maker");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Fractal: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addFractal)(cookieBaker);
            console.log("Successfully minted fractal");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Javascript: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addJavascript)(cookieBaker);
            console.log("Successfully minted javascript");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Idleverse: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addIdleverse)(cookieBaker);
            console.log("Successfully minted idleverse");
            saveState(source, sourceValue);
            break;
        }
        case actions_1.actions.incr_Cordex: {
            //action successful, update state
            sourceValue.cookieBaker = (0, state_1.addCordex)(cookieBaker);
            console.log("Successfully minted cordex");
            saveState(source, sourceValue);
            break;
        }
    }
};
(0, deku_js_interop_1.main)({}, transition);
