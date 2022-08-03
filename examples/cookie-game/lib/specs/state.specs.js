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
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            var cookie_baker = (0, state_1.addCookie)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore + 1
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
     * cursor
     */
    test('add cookie only mint one cursor', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cookiesBefore = cookieBaker.numberOfCookie;
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            cookieBaker.numberOfCookie = cookiesBefore + cursorCostBefore;
            var cookie_baker = (0, state_1.addCursor)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore + 1
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost > cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps > cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
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
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we have enough cookies to buy a grandma
            cookieBaker.numberOfCookie = cookiesBefore + grandmaCostBefore;
            var cookie_baker = (0, state_1.addGrandma)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore + 1
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost > grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps > grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
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
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we have enough cookies to buy a farm
            cookieBaker.numberOfCookie = cookiesBefore + farmCostBefore;
            var cookie_baker = (0, state_1.addFarm)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore + 1
                && cookieBaker.numberOfMine === minesBefore
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost > farmCostBefore
                && cookieBaker.mineCost === mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps > farmCpsBefore
                && cookieBaker.mineCps === mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
    /**
    * mine
    */
    test('add mine only mint one mine, decrease cookie amount, increase mine cost, and increase mines CPS', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cookiesBefore = cookieBaker.numberOfCookie;
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we have enough cookies to buy a farm
            cookieBaker.numberOfCookie = cookiesBefore + mineCostBefore;
            var cookie_baker = (0, state_1.addMine)(cookieBaker);
            return (cookieBaker.numberOfCookie === cookiesBefore
                && cookieBaker.numberOfCursor === cursorsBefore
                && cookieBaker.numberOfGrandma === grandmasBefore
                && cookieBaker.numberOfFarm === farmsBefore
                && cookieBaker.numberOfMine === minesBefore + 1
                && cookieBaker.numberOfFactory === factoriesBefore
                && cookieBaker.numberOfFreeCursor === freeCursorBefore
                && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                && cookieBaker.numberOfFreeFarm === freeFarmBefore
                && cookieBaker.numberOfFreeMine === freeMineBefore
                && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                && cookieBaker.cursorCost === cursorCostBefore
                && cookieBaker.grandmaCost === grandmaCostBefore
                && cookieBaker.farmCost === farmCostBefore
                && cookieBaker.mineCost > mineCostBefore
                && cookieBaker.factoryCost === factoryCostBefore
                && cookieBaker.cursorCps === cursorCpsBefore
                && cookieBaker.grandmaCps === grandmaCpsBefore
                && cookieBaker.farmCps === farmCpsBefore
                && cookieBaker.mineCps > mineCpsBefore
                && cookieBaker.factoryCps === factoryCpsBefore);
        }), { verbose: true });
    });
    /**
     * factory
     */
    /*test('add factory only mint one factory, decrease cookie amount, increase factory cost, and increase factories CPS', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
                    const cookiesBefore = cookieBaker.numberOfCookie;
                    const cursorsBefore = cookieBaker.numberOfCursor;
                    const grandmasBefore = cookieBaker.numberOfGrandma;
                    const farmsBefore = cookieBaker.numberOfFarm;
                    const minesBefore = cookieBaker.numberOfMine;
                    const factoriesBefore = cookieBaker.numberOfFactory;
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const freeMineBefore = cookieBaker.numberOfFreeMine;
                    const freeFactoryBefore = cookieBaker.numberOfFreeFactory;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const mineCostBefore = cookieBaker.mineCost;
                    const factoryCostBefore = cookieBaker.factoryCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    const mineCpsBefore = cookieBaker.mineCps;
                    const factoryCpsBefore = cookieBaker.factoryCps;
                    //make sure we have enough cookies to buy a factory
                    cookieBaker.numberOfCookie = cookiesBefore + factoryCostBefore;
                    const cookie_baker = addFactory(cookieBaker);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore + 1
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost > factoryCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps > factoryCpsBefore)
                }), { verbose: true });
    });*/
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
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we can't buy a cursor
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addCursor)(cookieBaker);
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
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we can't buy a grandma
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addGrandma)(cookieBaker);
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
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we can't buy a farm
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addFarm)(cookieBaker);
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
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we can't buy a mine
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addMine)(cookieBaker);
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
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.numberOfCursor;
            var grandmasBefore = cookieBaker.numberOfGrandma;
            var farmsBefore = cookieBaker.numberOfFarm;
            var minesBefore = cookieBaker.numberOfMine;
            var factoriesBefore = cookieBaker.numberOfFactory;
            var freeCursorBefore = cookieBaker.numberOfFreeCursor;
            var freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
            var freeFarmBefore = cookieBaker.numberOfFreeFarm;
            var freeMineBefore = cookieBaker.numberOfFreeMine;
            var freeFactoryBefore = cookieBaker.numberOfFreeFactory;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            //make sure we can't buy a factory
            cookieBaker.numberOfCookie = 0;
            var cookie_baker = (0, state_1.addFactory)(cookieBaker);
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
