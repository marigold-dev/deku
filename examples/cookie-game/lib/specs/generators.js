"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.cookieBakerArbitrary = void 0;
var fc = require("fast-check");
var state_1 = require("../src/state");
var cookieBakerArbitrary = function () {
    return fc.tuple(fc.integer({ min: 0, max: 2000000000 }), fc.integer({ min: 0, max: 200 }), fc.integer({ min: 0, max: 200 }), fc.integer({ min: 0, max: 200 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 })).map(function (_a) {
        var numberOfCookie = _a[0], numberOfCursor = _a[1], numberOfGrandma = _a[2], numberOfFarm = _a[3], numberOfMine = _a[4], numberOfFactory = _a[5], numberOfFreeCursor = _a[6], numberOfFreeGrandma = _a[7], numberOfFreeFarm = _a[8], numberOfFreeMine = _a[9], numberOfFreeFactory = _a[10];
        var cookie_baker = (0, state_1.createCookieBaker)(numberOfCookie, numberOfCursor, numberOfGrandma, numberOfFarm, numberOfMine, numberOfFactory, numberOfFreeCursor, numberOfFreeGrandma, numberOfFreeFarm, numberOfFreeMine, numberOfFreeFactory);
        return cookie_baker;
    });
};
exports.cookieBakerArbitrary = cookieBakerArbitrary;
