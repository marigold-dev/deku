import * as fc from 'fast-check';
import { cookieBakerArbitrary } from './generators'
import { addCookie, addCursor, addGrandma, addFarm, addMine, addFactory } from '../src/state'

describe('cookieBaker.add_XXX successful', () => {
    test('add cookie only mint one cookie', () => {
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
                    const cookie_baker = addCookie(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });

    /**
     * cursor
     */
    /*test('add cookie only mint one cursor', () => {
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
                    cookieBaker.numberOfCookie = cookiesBefore + cursorCostBefore;
                    const cookie_baker = addCursor(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });*/

    /**
     * grandma
     */
    test('add grandma only mint one grandma, decrease cookie amount, increase grandma cost, and increase grandmas CPS', () => {
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
                    const factoryCpsBefore = cookieBaker.factoryCps
                    //make sure we have enough cookies to buy a grandma
                    cookieBaker.numberOfCookie = cookiesBefore + grandmaCostBefore;
                    const cookie_baker = addGrandma(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });

    /**
     * farm
     */
    test('add farm only mint one farm, decrease cookie amount, increase farm cost, and increase farms CPS', () => {
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
                    //make sure we have enough cookies to buy a farm
                    cookieBaker.numberOfCookie = cookiesBefore + farmCostBefore;
                    const cookie_baker = addFarm(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });


     /**
     * mine
     */
    test('add mine only mint one mine, decrease cookie amount, increase mine cost, and increase mines CPS', () => {
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
                    //make sure we have enough cookies to buy a farm
                    cookieBaker.numberOfCookie = cookiesBefore + mineCostBefore;
                    const cookie_baker = addMine(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
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

describe('cookieBaker.add_XXX without enough', () => {
    /**
     * Cursor
     */
    test('Cannot mint cursor if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
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
                    //make sure we can't buy a cursor
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addCursor(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });
    /**
     * Grandma
     */
    test('Cannot mint grandma if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
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
                    //make sure we can't buy a grandma
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addGrandma(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });
    /**
     * Farm
     */
    test('Cannot mint farm if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
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
                    //make sure we can't buy a farm
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addFarm(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });
    /**
     * Mine
     */
    test('Cannot mint mine if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
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
                    //make sure we can't buy a mine
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addMine(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });
    /**
     * Factory
     */
    test('Cannot mint factory if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
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
                    //make sure we can't buy a factory
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addFactory(cookieBaker);
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
                        && cookieBaker.factoryCps === factoryCpsBefore)
                }), { verbose: true });
    });
});
