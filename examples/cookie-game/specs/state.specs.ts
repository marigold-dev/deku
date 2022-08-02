import * as fc from 'fast-check';
import { cookieBakerArbitrary } from './generators'
import { addCookie, addCursor, addGrandma, addFarm } from '../src/state'

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
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    const cookie_baker = addCookie(cookieBaker);
                    return (cookieBaker.numberOfCookie === cookiesBefore + 1
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore)
                }), { verbose: true });
    });

    /**
     * Cursor
     */
    test('add cursor only mint one cursor, decrease cookie amount, increase cursor cost, and increase cursor CPS', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
                    const cookiesBefore = cookieBaker.numberOfCookie;
                    const cursorsBefore = cookieBaker.numberOfCursor;
                    const grandmasBefore = cookieBaker.numberOfGrandma;
                    const farmsBefore = cookieBaker.numberOfFarm;
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    //make sure we have enough cookies to buy a cursor
                    cookieBaker.numberOfCookie = cookiesBefore + cursorCostBefore;
                    const cookie_baker = addCursor(cookieBaker);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore + 1
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.cursorCost > cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.cursorCps > cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                    )
                }), { verbose: true });
    });
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
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    //make sure we have enough cookies to buy a grandma
                    cookieBaker.numberOfCookie = cookiesBefore + grandmaCostBefore;
                    const cookie_baker = addGrandma(cookieBaker);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore + 1
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost > grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps > grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore)
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
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    //make sure we have enough cookies to buy a farm
                    cookieBaker.numberOfCookie = cookiesBefore + farmCostBefore;
                    const cookie_baker = addFarm(cookieBaker);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore + 1
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost > farmCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps > farmCpsBefore)
                }), { verbose: true });
    });
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
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    //make sure we can't buy a cursor
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addCursor(cookieBaker);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore)
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
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    //make sure we can't buy a grandma
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addGrandma(cookieBaker);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore)
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
                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const cursorCostBefore = cookieBaker.cursorCost;
                    const grandmaCostBefore = cookieBaker.grandmaCost;
                    const farmCostBefore = cookieBaker.farmCost;
                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    //make sure we can't buy a farm
                    cookieBaker.numberOfCookie = 0
                    const cookie_baker = addFarm(cookieBaker);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore)
                }), { verbose: true });
    });
});
