"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.cookieBakerArbitrary = void 0;
var fc = require("fast-check");
var state_1 = require("../src/state");
// 77
var cookieBakerArbitrary = function () {
    return fc.tuple(fc.integer({ min: 0, max: 2000000000 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 })).map(function (_a) {
        var cookies = _a[0], cursors = _a[1], grandmas = _a[2], farms = _a[3], mines = _a[4], factories = _a[5], banks = _a[6], temples = _a[7], wizards = _a[8], shipments = _a[9], alchemies = _a[10], portals = _a[11], timeMachines = _a[12], antimatters = _a[13], prisms = _a[14], chanceMakers = _a[15], fractals = _a[16], javaScripts = _a[17], idleverses = _a[18], cordexs = _a[19], freeCursor = _a[20], freeGrandma = _a[21], freeFarm = _a[22], freeMine = _a[23], freeFactory = _a[24], freeBank = _a[25], freeTemple = _a[26], freeWizard = _a[27], freeShipment = _a[28], freeAlchemy = _a[29], freePortal = _a[30], freeTimeMachine = _a[31], freeAntimatter = _a[32], freePrism = _a[33], freeChanceMaker = _a[34], freeFractal = _a[35], freeJavaScript = _a[36], freeIdleverse = _a[37], freeCordex = _a[38];
        var cookie_baker = (0, state_1.createCookieBaker)(cookies, cursors, grandmas, farms, mines, factories, banks, temples, wizards, shipments, alchemies, portals, timeMachines, antimatters, prisms, chanceMakers, fractals, javaScripts, idleverses, cordexs, freeCursor, freeGrandma, freeFarm, freeMine, freeFactory, freeBank, freeTemple, freeWizard, freeShipment, freeAlchemy, freePortal, freeTimeMachine, freeAntimatter, freePrism, freeChanceMaker, freeFractal, freeJavaScript, freeIdleverse, freeCordex);
        return cookie_baker;
    });
};
exports.cookieBakerArbitrary = cookieBakerArbitrary;
