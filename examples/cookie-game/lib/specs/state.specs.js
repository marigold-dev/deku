"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fc = require("fast-check");
var generators_1 = require("./generators");
var state_1 = require("../src/state");
describe('cookieBaker.add_XXX successful', function () {
    test('add cookie only mint one cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cookiesBefore = cookieBaker.numberOfCookie;
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var free_cursorBefore = cookieBaker.numberOfFreeCursor;
            var free_grandmaBefore = cookieBaker.numberOfFreeGrandma;
            var free_farmBefore = cookieBaker.numberOfFreeFarm;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var cookie_baker = (0, state_1.addCookie)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore + 1
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfFreeCursor === free_cursorBefore
                && cookieBaker.numberOfFreeGrandma === free_grandmaBefore
                && cookieBaker.numberOfFreeFarm === free_farmBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore);
        }), { verbose: true });
    });
    /**
     * Cursor
     */
    test('add cursor only mint one cursor, decrease cookie amount, increase cursor cost, and increase cursor CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cookiesBefore = cookieBaker.numberOfCookie;
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var free_cursorBefore = cookieBaker.numberOfFreeCursor;
            var free_grandmaBefore = cookieBaker.numberOfFreeGrandma;
            var free_farmBefore = cookieBaker.numberOfFreeFarm;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            //make sure we have enough cookies to buy a cursor
            cookieBaker.numberOfCookie = cookiesBefore + cursorCostBefore;
            var cookie_baker = (0, state_1.addCursor)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore + 1
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfFreeCursor === free_cursorBefore
                && cookieBaker.numberOfFreeGrandma === free_grandmaBefore
                && cookieBaker.numberOfFreeFarm === free_farmBefore
                && cookieBaker.cursorCost > cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.cursorCps > cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore);
        }), { verbose: true });
    });
    /**
     * grandma
     */
    test('add grandma only mint one grandma, decrease cookie amount, increase grandma cost, and increase grandmas CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cookiesBefore = cookieBaker.numberOfCookie;
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var free_cursorBefore = cookieBaker.numberOfFreeCursor;
            var free_grandmaBefore = cookieBaker.numberOfFreeGrandma;
            var free_farmBefore = cookieBaker.numberOfFreeFarm;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            //make sure we have enough cookies to buy a grandma
            cookieBaker.numberOfCookie = cookiesBefore + grandmaCostBefore;
            var cookie_baker = (0, state_1.addGrandma)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore + 1
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfFreeCursor === free_cursorBefore
                && cookieBaker.numberOfFreeGrandma === free_grandmaBefore
                && cookieBaker.numberOfFreeFarm === free_farmBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost > grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps > grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore);
        }), { verbose: true });
    });
    /**
     * farm
     */
    test('add farm only mint one farm, decrease cookie amount, increase farm cost, and increase farms CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cookiesBefore = cookieBaker.numberOfCookie;
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var free_cursorBefore = cookieBaker.numberOfFreeCursor;
            var free_grandmaBefore = cookieBaker.numberOfFreeGrandma;
            var free_farmBefore = cookieBaker.numberOfFreeFarm;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            //make sure we have enough cookies to buy a farm
            cookieBaker.numberOfCookie = cookiesBefore + farmCostBefore;
            var cookie_baker = (0, state_1.addFarm)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore + 1
                && cookieBaker.numberOfFreeCursor === free_cursorBefore
                && cookieBaker.numberOfFreeGrandma === free_grandmaBefore
                && cookieBaker.numberOfFreeFarm === free_farmBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost > farmCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps > farmCpsBefore);
        }), { verbose: true });
    });
});
describe('cookieBaker.add_XXX without enough', function () {
    /**
     * Cursor
     */
    test('Cannot mint cursor if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var free_cursorBefore = cookieBaker.numberOfFreeCursor;
            var free_grandmaBefore = cookieBaker.numberOfFreeGrandma;
            var free_farmBefore = cookieBaker.numberOfFreeFarm;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            //make sure we can't buy a cursor
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addCursor)(cookieBaker);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfFreeCursor === free_cursorBefore
                && cookieBaker.numberOfFreeGrandma === free_grandmaBefore
                && cookieBaker.numberOfFreeFarm === free_farmBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore);
        }), { verbose: true });
    });
    /**
     * Grandma
     */
    test('Cannot mint grandma if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var free_cursorBefore = cookieBaker.numberOfFreeCursor;
            var free_grandmaBefore = cookieBaker.numberOfFreeGrandma;
            var free_farmBefore = cookieBaker.numberOfFreeFarm;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            //make sure we can't buy a grandma
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addGrandma)(cookieBaker);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfFreeCursor === free_cursorBefore
                && cookieBaker.numberOfFreeGrandma === free_grandmaBefore
                && cookieBaker.numberOfFreeFarm === free_farmBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore);
        }), { verbose: true });
    });
    /**
     * Farm
     */
    test('Cannot mint farm if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var free_cursorBefore = cookieBaker.numberOfFreeCursor;
            var free_grandmaBefore = cookieBaker.numberOfFreeGrandma;
            var free_farmBefore = cookieBaker.numberOfFreeFarm;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            //make sure we can't buy a farm
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addFarm)(cookieBaker);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfFreeCursor === free_cursorBefore
                && cookieBaker.numberOfFreeGrandma === free_grandmaBefore
                && cookieBaker.numberOfFreeFarm === free_farmBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore);
        }), { verbose: true });
    });
});
