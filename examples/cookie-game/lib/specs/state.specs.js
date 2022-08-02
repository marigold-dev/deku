"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fc = require("fast-check");
var generators_1 = require("./generators");
var state_1 = require("../src/state");
describe('cookie_baker.add_XXX successful', function () {
    test('add cookie only mint one cookie', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cookies_before = cookieBaker.number_of_cookie;
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            var cookie_baker = (0, state_1.add_cookie)(cookieBaker);
            return (cookie_baker.number_of_cookie === cookies_before + 1
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before
                && cookie_baker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
    /**
     * Cursor
     */
    test('add cursor only mint one cursor, decrease cookie amount, increase cursor cost, and increase cursor CPS', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cookies_before = cookieBaker.number_of_cookie;
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we have enough cookies to buy a cursor
            cookieBaker.number_of_cookie = cookies_before + cursor_cost_before;
            var cookie_baker = (0, state_1.add_cursor)(cookieBaker);
            return (cookie_baker.number_of_cookie === cookies_before
                && cookie_baker.number_of_cursor === cursors_before + 1
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before
                && cookieBaker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine == free_mine_before
                && cookie_baker.cursor_cost > cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps > cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
    /**
     * grandma
     */
    test('add grandma only mint one grandma, decrease cookie amount, increase grandma cost, and increase grandmas CPS', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cookies_before = cookieBaker.number_of_cookie;
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we have enough cookies to buy a grandma
            cookieBaker.number_of_cookie = cookies_before + grandma_cost_before;
            var cookie_baker = (0, state_1.add_grandma)(cookieBaker);
            return (cookie_baker.number_of_cookie === cookies_before
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before + 1
                && cookie_baker.number_of_farm === farms_before
                && cookie_baker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost > grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps > grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
    /**
     * farm
     */
    test('add farm only mint one farm, decrease cookie amount, increase farm cost, and increase farms CPS', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cookies_before = cookieBaker.number_of_cookie;
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we have enough cookies to buy a farm
            cookieBaker.number_of_cookie = cookies_before + farm_cost_before;
            var cookie_baker = (0, state_1.add_farm)(cookieBaker);
            return (cookie_baker.number_of_cookie === cookies_before
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before + 1
                && cookie_baker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost > farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps > farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
    /**
   * mine
   */
    test('add mine only mint one mine, decrease cookie amount, increase mine cost, and increase mine CPS', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cookies_before = cookieBaker.number_of_cookie;
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we have enough cookies to buy a farm
            cookieBaker.number_of_cookie = cookies_before + mine_cost_before;
            var cookie_baker = (0, state_1.add_mine)(cookieBaker);
            return (cookie_baker.number_of_cookie === cookies_before
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before
                && cookie_baker.number_of_mine === mine_before + 1
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost > mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps > mine_cps_before);
        }), { verbose: true });
    });
});
describe('cookie_baker.add_XXX without enough', function () {
    /**
     * Cursor
     */
    test('Cannot mint cursor if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we can't buy a cursor
            cookieBaker.number_of_cookie = 0;
            var cookie_baker = (0, state_1.add_cursor)(cookieBaker);
            return (cookie_baker.number_of_cookie === 0
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before
                && cookie_baker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
    /**
     * Grandma
     */
    test('Cannot mint grandma if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we can't buy a grandma
            cookieBaker.number_of_cookie = 0;
            var cookie_baker = (0, state_1.add_grandma)(cookieBaker);
            return (cookie_baker.number_of_cookie === 0
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before
                && cookie_baker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
    /**
     * Farm
     */
    test('Cannot mint farm if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we can't buy a farm
            cookieBaker.number_of_cookie = 0;
            var cookie_baker = (0, state_1.add_farm)(cookieBaker);
            return (cookie_baker.number_of_cookie === 0
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before
                && cookie_baker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
    /**
     * Mine
     */
    test('Cannot mint mine if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookie_baker_arbitrary)(), function (cookieBaker) {
            var cursors_before = cookieBaker.number_of_cursor;
            var grandmas_before = cookieBaker.number_of_grandma;
            var farms_before = cookieBaker.number_of_farm;
            var mine_before = cookieBaker.number_of_mine;
            var free_cursor_before = cookieBaker.number_of_free_cursor;
            var free_grandma_before = cookieBaker.number_of_free_grandma;
            var free_farm_before = cookieBaker.number_of_free_farm;
            var free_mine_before = cookieBaker.number_of_free_mine;
            var cursor_cost_before = cookieBaker.cursor_cost;
            var grandma_cost_before = cookieBaker.grandma_cost;
            var farm_cost_before = cookieBaker.farm_cost;
            var mine_cost_before = cookieBaker.mine_cost;
            var cursor_cps_before = cookieBaker.cursor_cps;
            var grandma_cps_before = cookieBaker.grandma_cps;
            var farm_cps_before = cookieBaker.farm_cps;
            var mine_cps_before = cookieBaker.mine_cps;
            //make sure we can't buy a farm
            cookieBaker.number_of_cookie = 0;
            var cookie_baker = (0, state_1.add_mine)(cookieBaker);
            return (cookie_baker.number_of_cookie === 0
                && cookie_baker.number_of_cursor === cursors_before
                && cookie_baker.number_of_grandma === grandmas_before
                && cookie_baker.number_of_farm === farms_before
                && cookie_baker.number_of_mine === mine_before
                && cookie_baker.number_of_free_cursor === free_cursor_before
                && cookie_baker.number_of_free_grandma === free_grandma_before
                && cookie_baker.number_of_free_farm === free_farm_before
                && cookie_baker.number_of_free_mine === free_mine_before
                && cookie_baker.cursor_cost === cursor_cost_before
                && cookie_baker.grandma_cost === grandma_cost_before
                && cookie_baker.farm_cost === farm_cost_before
                && cookie_baker.mine_cost === mine_cost_before
                && cookie_baker.cursor_cps === cursor_cps_before
                && cookie_baker.grandma_cps === grandma_cps_before
                && cookie_baker.farm_cps === farm_cps_before
                && cookie_baker.mine_cps === mine_cps_before);
        }), { verbose: true });
    });
});
