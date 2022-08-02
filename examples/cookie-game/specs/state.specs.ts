import * as fc from 'fast-check';
import { cookie_baker_arbitrary } from './generators'
import { add_cookie, add_cursor, add_grandma, add_farm, add_mine, add_factory } from '../src/state'
import { factory } from 'typescript';

describe('cookie_baker.add_XXX successful', () => {
    test('add cookie only mint one cookie', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cookies_before = cookieBaker.number_of_cookie;
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    const cookie_baker = add_cookie(cookieBaker);
                    return (cookie_baker.number_of_cookie === cookies_before + 1
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });

    /**
     * Cursor
     */
    test('add cursor only mint one cursor, decrease cookie amount, increase cursor cost, and increase cursor CPS', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cookies_before = cookieBaker.number_of_cookie;
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we have enough cookies to buy a cursor
                    cookieBaker.number_of_cookie = cookies_before + cursor_cost_before;
                    const cookie_baker = add_cursor(cookieBaker);
                    return (cookie_baker.number_of_cookie === cookies_before
                        && cookie_baker.number_of_cursor === cursors_before + 1
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine == free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost > cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps > cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before
                    )
                }), { verbose: true });
    });
    /**
     * grandma
     */
    test('add grandma only mint one grandma, decrease cookie amount, increase grandma cost, and increase grandmas CPS', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cookies_before = cookieBaker.number_of_cookie;
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we have enough cookies to buy a grandma
                    cookieBaker.number_of_cookie = cookies_before + grandma_cost_before;
                    const cookie_baker = add_grandma(cookieBaker);
                    return (cookie_baker.number_of_cookie === cookies_before
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before + 1
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost > grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps > grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });

    /**
     * farm
     */
    test('add farm only mint one farm, decrease cookie amount, increase farm cost, and increase farms CPS', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cookies_before = cookieBaker.number_of_cookie;
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we have enough cookies to buy a farm
                    cookieBaker.number_of_cookie = cookies_before + farm_cost_before;
                    const cookie_baker = add_farm(cookieBaker);
                    return (cookie_baker.number_of_cookie === cookies_before
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before + 1
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost > farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps > farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });

      /**
     * mine
     */
    test('add mine only mint one mine, decrease cookie amount, increase mine cost, and increase mine CPS', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cookies_before = cookieBaker.number_of_cookie;
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we have enough cookies to buy a farm
                    cookieBaker.number_of_cookie = cookies_before + mine_cost_before;
                    const cookie_baker = add_mine(cookieBaker);
                    return (cookie_baker.number_of_cookie === cookies_before
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before 
                        && cookie_baker.number_of_mine === mine_before + 1
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost > mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps > mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });

      /**
     * factory
     */
    test('add mine only mint one factory, decrease cookie amount, increase factory cost, and increase factory CPS', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cookies_before = cookieBaker.number_of_cookie;
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we have enough cookies to buy a farm
                    cookieBaker.number_of_cookie = cookies_before + factory_cost_before;
                    const cookie_baker = add_factory(cookieBaker);
                    return (cookie_baker.number_of_cookie === cookies_before
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before 
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before + 1
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost > factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps > factory_cps_before)
                }), { verbose: true });
    });
});

describe('cookie_baker.add_XXX without enough', () => {
    /**
     * Cursor
     */
    test('Cannot mint cursor if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we can't buy a cursor
                    cookieBaker.number_of_cookie = 0
                    const cookie_baker = add_cursor(cookieBaker);
                    return (cookie_baker.number_of_cookie === 0
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });
    /**
     * Grandma
     */
    test('Cannot mint grandma if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we can't buy a grandma
                    cookieBaker.number_of_cookie = 0
                    const cookie_baker = add_grandma(cookieBaker);
                    return (cookie_baker.number_of_cookie === 0
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });
    /**
     * Farm
     */
    test('Cannot mint farm if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we can't buy a farm
                    cookieBaker.number_of_cookie = 0
                    const cookie_baker = add_farm(cookieBaker);
                    return (cookie_baker.number_of_cookie === 0
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });

    /**
     * Mine
     */
    test('Cannot mint mine if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we can't buy a farm
                    cookieBaker.number_of_cookie = 0
                    const cookie_baker = add_mine(cookieBaker);
                    return (cookie_baker.number_of_cookie === 0
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });


    /**
     * Factory
     */
    test('Cannot mint factory if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookie_baker_arbitrary(),
                cookieBaker => {
                    const cursors_before = cookieBaker.number_of_cursor;
                    const grandmas_before = cookieBaker.number_of_grandma;
                    const farms_before = cookieBaker.number_of_farm;
                    const mine_before = cookieBaker.number_of_mine;
                    const factory_before = cookieBaker.number_of_factory;
                    const free_cursor_before = cookieBaker.number_of_free_cursor;
                    const free_grandma_before = cookieBaker.number_of_free_grandma;
                    const free_farm_before = cookieBaker.number_of_free_farm;
                    const free_mine_before = cookieBaker.number_of_free_mine;
                    const free_factory_before = cookieBaker.number_of_free_factory;
                    const cursor_cost_before = cookieBaker.cursor_cost;
                    const grandma_cost_before = cookieBaker.grandma_cost;
                    const farm_cost_before = cookieBaker.farm_cost;
                    const mine_cost_before = cookieBaker.mine_cost;
                    const factory_cost_before = cookieBaker.factory_cost;
                    const cursor_cps_before = cookieBaker.cursor_cps;
                    const grandma_cps_before = cookieBaker.grandma_cps;
                    const farm_cps_before = cookieBaker.farm_cps;
                    const mine_cps_before = cookieBaker.mine_cps;
                    const factory_cps_before = cookieBaker.factory_cps;
                    //make sure we can't buy a farm
                    cookieBaker.number_of_cookie = 0
                    const cookie_baker = add_factory(cookieBaker);
                    return (cookie_baker.number_of_cookie === 0
                        && cookie_baker.number_of_cursor === cursors_before
                        && cookie_baker.number_of_grandma === grandmas_before
                        && cookie_baker.number_of_farm === farms_before
                        && cookie_baker.number_of_mine === mine_before
                        && cookie_baker.number_of_factory === factory_before
                        && cookie_baker.number_of_free_cursor === free_cursor_before
                        && cookie_baker.number_of_free_grandma === free_grandma_before
                        && cookie_baker.number_of_free_farm === free_farm_before
                        && cookie_baker.number_of_free_mine === free_mine_before
                        && cookie_baker.number_of_free_factory === free_factory_before
                        && cookie_baker.cursor_cost === cursor_cost_before
                        && cookie_baker.grandma_cost === grandma_cost_before
                        && cookie_baker.farm_cost === farm_cost_before
                        && cookie_baker.mine_cost === mine_cost_before
                        && cookie_baker.factory_cost === factory_cost_before
                        && cookie_baker.cursor_cps === cursor_cps_before
                        && cookie_baker.grandma_cps === grandma_cps_before
                        && cookie_baker.farm_cps === farm_cps_before
                        && cookie_baker.mine_cps === mine_cps_before
                        && cookie_baker.factory_cps === factory_cps_before)
                }), { verbose: true });
    });
});
