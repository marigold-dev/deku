"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.cookieBakerArbitrary = void 0;
const fc = require("fast-check");
const state_1 = require("../src/state");
// 77
const cookieBakerArbitrary = () => fc.tuple(fc.bigInt({ min: 0n, max: 2000000000n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n }), fc.bigInt({ min: 0n, max: 10n })).map(([cookies, cursors, grandmas, farms, mines, factories, banks, temples, wizards, shipments, alchemies, portals, timeMachines, antimatters, prisms, chanceMakers, fractals, javaScripts, idleverses, cordexs, eatenCookies]) => {
    const cookie_baker = (0, state_1.createCookieBaker)(cookies, cursors, grandmas, farms, mines, factories, banks, temples, wizards, shipments, alchemies, portals, timeMachines, antimatters, prisms, chanceMakers, fractals, javaScripts, idleverses, cordexs, eatenCookies);
    return cookie_baker;
});
exports.cookieBakerArbitrary = cookieBakerArbitrary;
