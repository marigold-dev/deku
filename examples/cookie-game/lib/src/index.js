"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
var deku_js_interop_1 = require("deku_js_interop");
var state_1 = require("./state");
var actions_1 = require("./actions");
var saveState = function (source, sourceValue) {
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
    var cookieBaker = (0, state_1.createCookieBaker)(sourceValue.cookieBaker.cookies, sourceValue.cookieBaker.cursors, sourceValue.cookieBaker.grandmas, sourceValue.cookieBaker.farms, sourceValue.cookieBaker.mines, sourceValue.cookieBaker.factories, sourceValue.cookieBaker.banks, sourceValue.cookieBaker.temples, sourceValue.cookieBaker.wizards, sourceValue.cookieBaker.shipments, sourceValue.cookieBaker.alchemies, sourceValue.cookieBaker.portals, sourceValue.cookieBaker.timeMachines, sourceValue.cookieBaker.antimatters, sourceValue.cookieBaker.prisms, sourceValue.cookieBaker.chanceMakers, sourceValue.cookieBaker.fractals, sourceValue.cookieBaker.javaScripts, sourceValue.cookieBaker.idleverses, sourceValue.cookieBaker.cordexs, sourceValue.cookieBaker.freeCursor, sourceValue.cookieBaker.freeGrandma, sourceValue.cookieBaker.freeFarm, sourceValue.cookieBaker.freeMine, sourceValue.cookieBaker.freeFactory, sourceValue.cookieBaker.freeBank, sourceValue.cookieBaker.freeTemple, sourceValue.cookieBaker.freeWizard, sourceValue.cookieBaker.freeShipment, sourceValue.cookieBaker.freeAlchemy, sourceValue.cookieBaker.freePortal, sourceValue.cookieBaker.freeTimeMachine, sourceValue.cookieBaker.freeAntimatter, sourceValue.cookieBaker.freePrism, sourceValue.cookieBaker.freeChanceMaker, sourceValue.cookieBaker.freeFractal, sourceValue.cookieBaker.FreeJavaScript, sourceValue.cookieBaker.freeIdleverse, sourceValue.cookieBaker.freeCordex);
    switch (operation) {
        // TODO: wallet address
        case actions_1.actions.address: {
            console.log("address");
            saveState(source, sourceValue);
            break;
        }
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
(0, deku_js_interop_1.main)(
//tz address must be replaced by a correct one obtained with 
//deku-cli create-wallet
{
    // make it as an empty string, to fetch from payload
    //"tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc":
    "": {
        cookieBaker: {
            cookies: 0,
            cursors: 0.,
            grandmas: 0.,
            farms: 0.,
            mines: 0.,
            factories: 0.,
            banks: 0.,
            temples: 0.,
            wizards: 0.,
            shipments: 0.,
            alchemies: 0.,
            portals: 0.,
            timeMachines: 0.,
            antimatters: 0.,
            prisms: 0.,
            chanceMakers: 0.,
            fractals: 0.,
            javaScripts: 0.,
            idleverses: 0.,
            cordexs: 0.,
            freeCursor: 0,
            freeGrandma: 0,
            freeFarm: 0,
            freeMine: 0,
            freeFactory: 0,
            freeBank: 0,
            freeTemple: 0,
            freeWizard: 0,
            freeShipment: 0,
            freeAlchemy: 0,
            freePortal: 0,
            freeTimeMachine: 0,
            freeAntimatter: 0,
            freePrism: 0,
            freeChanceMaker: 0,
            freeFractal: 0,
            freeJavaScript: 0,
            freeIdleverse: 0,
            freeCordex: 0,
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
