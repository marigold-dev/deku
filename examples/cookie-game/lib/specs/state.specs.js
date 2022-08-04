"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fc = require("fast-check");
var generators_1 = require("./generators");
var state_1 = require("../src/state");
describe('cookieBaker.add_XXX successful', function () {
    test('add cookie only mint one cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cookiesBefore = cookieBakerType.numberOfCookie;
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var banksBefore = cookieBakerType.numberOfBank;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var freeBankBefore = cookieBakerType.numberOfFreeBank;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var bankCostBefore = cookieBakerType.bankCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            var bankCpsBefore = cookieBakerType.bankCps;
            var cookieBaker = (0, state_1.addCookie)(cookieBakerType);
            return (cookieBaker.numberOfCookie === cookiesBefore + 1
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfBank === banksBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.numberOfFreeBank === freeBankBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.bankCost === bankCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore
                && cookieBaker.bankCps === bankCpsBefore);
        }), { verbose: true });
    });
    /**
     * cursor
     */
    test('add cookie only mint one cursor', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cookiesBefore = cookieBakerType.numberOfCookie;
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var banksBefore = cookieBakerType.numberOfBank;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var freeBankBefore = cookieBakerType.numberOfFreeBank;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var bankCostBefore = cookieBakerType.bankCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            var bankCpsBefore = cookieBakerType.bankCps;
            cookieBakerType.numberOfCookie = cookiesBefore + cursorCostBefore;
            var cookieBaker = (0, state_1.addCursor)(cookieBakerType);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore + 1
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfBank === banksBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.numberOfFreeBank === freeBankBefore
                && cookieBaker.cursorCost > cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.bankCost === bankCostBefore
                && cookieBaker.cursorCps > cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore
                && cookieBaker.bankCps === bankCpsBefore);
        }), { verbose: true });
    });
    /**
     * grandma
     */
    test('add grandma only mint one grandma, decrease cookie amount, increase grandma cost, and increase grandmas CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cookiesBefore = cookieBakerType.numberOfCookie;
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var banksBefore = cookieBakerType.numberOfBank;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var freeBankBefore = cookieBakerType.numberOfFreeBank;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var bankCostBefore = cookieBakerType.bankCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            var bankCpsBefore = cookieBakerType.bankCps;
            //make sure we have enough cookies to buy a grandma
            cookieBakerType.numberOfCookie = cookiesBefore + grandmaCostBefore;
            var cookieBaker = (0, state_1.addGrandma)(cookieBakerType);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore + 1
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfBank === banksBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.numberOfFreeBank === freeBankBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost > grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.bankCost === bankCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps > grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore
                && cookieBaker.bankCps === bankCpsBefore);
        }), { verbose: true });
    });
    /**
     * farm
     */
    test('add farm only mint one farm, decrease cookie amount, increase farm cost, and increase farms CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cookiesBefore = cookieBakerType.numberOfCookie;
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var banksBefore = cookieBakerType.numberOfBank;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var freeBankBefore = cookieBakerType.numberOfFreeBank;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var bankCostBefore = cookieBakerType.bankCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            var bankCpsBefore = cookieBakerType.bankCps;
            //make sure we have enough cookies to buy a farm
            cookieBakerType.numberOfCookie = cookiesBefore + farmCostBefore;
            var cookieBaker = (0, state_1.addFarm)(cookieBakerType);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore + 1
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfBank === banksBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.numberOfFreeBank === freeBankBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost > farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.bankCost === bankCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps > farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore
                && cookieBaker.bankCps === bankCpsBefore);
        }), { verbose: true });
    });
    /**
    * mine
    */
    test('add mine only mint one mine, decrease cookie amount, increase mine cost, and increase mines CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cookiesBefore = cookieBakerType.numberOfCookie;
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var banksBefore = cookieBakerType.numberOfBank;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var freeBankBefore = cookieBakerType.numberOfFreeBank;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var bankCostBefore = cookieBakerType.bankCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            var bankCpsBefore = cookieBakerType.bankCps;
            //make sure we have enough cookies to buy a farm
            cookieBakerType.numberOfCookie = cookiesBefore + mineCostBefore;
            var cookieBaker = (0, state_1.addMine)(cookieBakerType);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore + 1
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfBank === banksBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.numberOfFreeBank === freeBankBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost > mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.bankCost === bankCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps > mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore
                && cookieBaker.bankCps === bankCpsBefore);
        }), { verbose: true });
    });
    /**
     * factory
     */
    test('add factory only mint one factory, decrease cookie amount, increase factory cost, and increase factories CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cookiesBefore = cookieBakerType.numberOfCookie;
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var banksBefore = cookieBakerType.numberOfBank;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var freeBankBefore = cookieBakerType.numberOfFreeBank;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var bankCostBefore = cookieBakerType.bankCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            var bankCpsBefore = cookieBakerType.bankCps;
            //make sure we have enough cookies to buy a factory
            cookieBakerType.numberOfCookie = cookiesBefore + factoryCostBefore;
            var cookieBaker = (0, state_1.addFactory)(cookieBakerType);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore + 1
                && cookieBaker.numberOfBank === banksBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.numberOfFreeBank === freeBankBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost > factoryCostBefore
                && cookieBaker.bankCost === bankCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps > factoryCpsBefore
                && cookieBaker.bankCps === bankCpsBefore);
        }), { verbose: true });
    });
    /**
     * bank
     */
    test('add bank only mint one bank, decrease cookie amount, increase bank cost, and increase banks CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cookiesBefore = cookieBakerType.numberOfCookie;
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var banksBefore = cookieBakerType.numberOfBank;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var freeBankBefore = cookieBakerType.numberOfFreeBank;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var bankCostBefore = cookieBakerType.bankCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            var bankCpsBefore = cookieBakerType.bankCps;
            //make sure we have enough cookies to buy a bank
            cookieBakerType.numberOfCookie = cookiesBefore + bankCostBefore;
            var cookieBaker = (0, state_1.addBank)(cookieBakerType);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfBank === banksBefore + 1
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.numberOfFreeBank === freeBankBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.bankCost > bankCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore
                && cookieBaker.bankCps > bankCpsBefore);
        }), { verbose: true });
    });
});
describe('cookieBaker.add_XXX without enough', function () {
    /**
     * Cursor
     */
    test('Cannot mint cursor if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            //make sure we can't buy a cursor
            cookieBakerType.numberOfCookie = 0;
            var cookieBaker = (0, state_1.addCursor)(cookieBakerType);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
    /**
     * Grandma
     */
    test('Cannot mint grandma if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            //make sure we can't buy a grandma
            cookieBakerType.numberOfCookie = 0;
            var cookieBaker = (0, state_1.addGrandma)(cookieBakerType);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
    /**
     * Farm
     */
    test('Cannot mint farm if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            //make sure we can't buy a farm
            cookieBakerType.numberOfCookie = 0;
            var cookieBaker = (0, state_1.addFarm)(cookieBakerType);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
    /**
     * Mine
     */
    test('Cannot mint mine if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            //make sure we can't buy a mine
            cookieBakerType.numberOfCookie = 0;
            var cookieBaker = (0, state_1.addMine)(cookieBakerType);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
    /**
     * Factory
     */
    test('Cannot mint factory if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            //make sure we can't buy a factory
            cookieBakerType.numberOfCookie = 0;
            var cookieBaker = (0, state_1.addFactory)(cookieBakerType);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
    /**
     * Bank
     */
    test('Cannot mint bank if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBakerType) {
            var cursorsBefore = cookieBakerType.numberOfCursor;
            var grandmasBefore = cookieBakerType.numberOfGrandma;
            var farmsBefore = cookieBakerType.numberOfFarm;
            var minesBefore = cookieBakerType.numberOfMine;
            var factoriesBefore = cookieBakerType.numberOfFactory;
            var freeCursorBefore = cookieBakerType.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
            var freeFarmBefore = cookieBakerType.numberOfFreeFarm;
            var freeMineBefore = cookieBakerType.numberOfFreeMine;
            var freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
            var cursorCostBefore = cookieBakerType.cursorCost;
            var grandmaCostBefore = cookieBakerType.grandmaCost;
            var farmCostBefore = cookieBakerType.farmCost;
            var mineCostBefore = cookieBakerType.mineCost;
            var factoryCostBefore = cookieBakerType.factoryCost;
            var cursorCpsBefore = cookieBakerType.cursorCps;
            var grandmaCpsBefore = cookieBakerType.grandmaCps;
            var farmCpsBefore = cookieBakerType.farmCps;
            var mineCpsBefore = cookieBakerType.mineCps;
            var factoryCpsBefore = cookieBakerType.factoryCps;
            //make sure we can't buy a factory
            cookieBakerType.numberOfCookie = 0;
            var cookieBaker = (0, state_1.addBank)(cookieBakerType);
            return (cookieBaker.numberOfCookie === 0
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
});
