"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.add_mine = exports.add_farm = exports.add_grandma = exports.add_cursor = exports.add_cookie = exports.calculate_cost = exports.create_cookie_baker = exports.initial_mine_cost = exports.initial_farm_cost = exports.initial_grandma_cost = exports.initial_cursor_cost = exports.initial_mine_cps = exports.initial_farm_cps = exports.initial_grandma_cps = exports.initial_cursor_cps = void 0;
var actions_1 = require("./actions");
/*
   Understanding rule of game:
   https://cookieclicker.fandom.com/wiki/Building
   - cookie: is the number of cookie that one have
   - cursor: this level only available when you have enough number
             of cookie required (for instance: it is 15 cookie in this example).
             When a cursor is added, the number of cookie will be removed by 15.
             - The first cursor will have the initial cost: 15,
             after that it will be increase by this formula:
             new_cursor = number_of_cursor - number_of_free_cursor
             cost = floor (init_cost * power (1.15, new_cursor))
             - A cps: is the cookie per second and it is :
             cursor_cps = number_of_cursor * init_cursor_cps
   - grandma: same as cursor, it requires 100 cookies.
   - farm: it requires 1100 cookies
*/
exports.initial_cursor_cps = 0.1;
exports.initial_grandma_cps = 1;
exports.initial_farm_cps = 8;
exports.initial_mine_cps = 47;
exports.initial_cursor_cost = 15;
exports.initial_grandma_cost = 100;
exports.initial_farm_cost = 1100;
exports.initial_mine_cost = 12000;
var create_cookie_baker = function (number_of_cookie, number_of_cursor, number_of_grandma, number_of_farm, number_of_mine, number_of_free_cursor, number_of_free_grandma, number_of_free_farm, number_of_free_mine) {
    var cookie_baker = {
        number_of_cookie: number_of_cookie,
        number_of_cursor: number_of_cursor,
        number_of_grandma: number_of_grandma,
        number_of_farm: number_of_farm,
        number_of_mine: number_of_mine,
        number_of_free_cursor: number_of_free_cursor,
        number_of_free_grandma: number_of_free_grandma,
        number_of_free_farm: number_of_free_farm,
        number_of_free_mine: number_of_free_mine,
        cursor_cost: 0,
        grandma_cost: 0,
        farm_cost: 0,
        mine_cost: 0,
        cursor_cps: 0,
        grandma_cps: 0,
        farm_cps: 0,
        mine_cps: 0,
    };
    var cursor_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_cursor, cookie_baker);
    var grandma_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_grandma, cookie_baker);
    var farm_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_farm, cookie_baker);
    var mine_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_mine, cookie_baker);
    var cursor_cps = cookie_baker.number_of_cursor * exports.initial_cursor_cps;
    var grandma_cps = cookie_baker.number_of_grandma * exports.initial_grandma_cps;
    var farm_cps = cookie_baker.number_of_farm * exports.initial_farm_cps;
    var mine_cps = cookie_baker.number_of_mine * exports.initial_mine_cps;
    cookie_baker.cursor_cost = cursor_cost;
    cookie_baker.grandma_cost = grandma_cost;
    cookie_baker.farm_cost = farm_cost;
    cookie_baker.mine_cost = mine_cost;
    cookie_baker.cursor_cps = cursor_cps;
    cookie_baker.grandma_cps = grandma_cps;
    cookie_baker.farm_cps = farm_cps;
    cookie_baker.mine_cps = mine_cps;
    return cookie_baker;
};
exports.create_cookie_baker = create_cookie_baker;
var calculate_cost = function (action, cookie_baker) {
    switch (action) {
        case actions_1.action_type.increment_cookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case actions_1.action_type.increment_cursor:
            console.log("Calculating price for next cursor, actual price is: " + cookie_baker.cursor_cost);
            var new_cursor_price = Math.floor(exports.initial_cursor_cost * Math.pow(1.15, cookie_baker.number_of_cursor - cookie_baker.number_of_free_cursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;
        case actions_1.action_type.increment_grandma:
            console.log("Calculating price for next grandma, actual price is: " + cookie_baker.grandma_cost);
            var new_grandma_price = Math.floor(exports.initial_grandma_cost * Math.pow(1.15, cookie_baker.number_of_grandma - cookie_baker.number_of_free_grandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;
        case actions_1.action_type.increment_farm:
            console.log("Calculating price for next farm, actual price is: " + cookie_baker.farm_cost);
            var new_farm_price = Math.floor(exports.initial_farm_cost * Math.pow(1.15, cookie_baker.number_of_farm - cookie_baker.number_of_free_farm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;
        case actions_1.action_type.increment_mine:
            console.log("Calculating price for next mine, actual price is: " + cookie_baker.mine_cost);
            var new_mine_pirce = Math.floor(exports.initial_mine_cost * Math.pow(1.15, cookie_baker.number_of_mine - cookie_baker.number_of_free_mine));
            console.log("New mine price is: " + new_mine_pirce);
            return new_mine_pirce;
    }
};
exports.calculate_cost = calculate_cost;
var add_cookie = function (cookie_baker) {
    console.log("Adding cookie: " + cookie_baker.number_of_cookie);
    cookie_baker.number_of_cookie = cookie_baker.number_of_cookie + 1;
    console.log("Successfully added cookie: " + cookie_baker.number_of_cookie);
    return cookie_baker;
};
exports.add_cookie = add_cookie;
var add_cursor = function (cookie_baker) {
    if (cookie_baker.number_of_cookie >= cookie_baker.cursor_cost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookie_baker.number_of_cursor = cookie_baker.number_of_cursor + 1;
        // removing cursor cost
        cookie_baker.number_of_cookie = cookie_baker.number_of_cookie - cookie_baker.cursor_cost;
        // calculating next cursor price
        cookie_baker.cursor_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_cursor, cookie_baker);
        // calculate new cps
        cookie_baker.cursor_cps = cookie_baker.number_of_cursor * exports.initial_cursor_cps;
        return cookie_baker;
    }
    else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookie_baker.cursor_cost + " actual amount: " + cookie_baker.number_of_cookie);
        return cookie_baker;
    }
};
exports.add_cursor = add_cursor;
var add_grandma = function (cookie_baker) {
    if (cookie_baker.number_of_cookie >= cookie_baker.grandma_cost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookie_baker.number_of_grandma = cookie_baker.number_of_grandma + 1;
        // removing grandma cost
        cookie_baker.number_of_cookie = cookie_baker.number_of_cookie - cookie_baker.grandma_cost;
        // calculating next grandma price
        cookie_baker.grandma_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_grandma, cookie_baker);
        // calculate new cps
        cookie_baker.grandma_cps = cookie_baker.number_of_grandma * exports.initial_grandma_cps;
        return cookie_baker;
    }
    else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookie_baker.grandma_cost + " actual amount: " + cookie_baker.number_of_cookie);
        return cookie_baker;
    }
};
exports.add_grandma = add_grandma;
var add_farm = function (cookie_baker) {
    if (cookie_baker.number_of_cookie >= cookie_baker.farm_cost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookie_baker.number_of_farm = cookie_baker.number_of_farm + 1;
        // removing farm cost
        cookie_baker.number_of_cookie = cookie_baker.number_of_cookie - cookie_baker.farm_cost;
        // calculating next farm price
        cookie_baker.farm_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_farm, cookie_baker);
        // calculate new cps
        cookie_baker.farm_cps = cookie_baker.number_of_farm * exports.initial_farm_cps;
        return cookie_baker;
    }
    else {
        console.log("Not enough cookie to buy a farm, needed: " + cookie_baker.farm_cost + " actual amount: " + cookie_baker.number_of_cookie);
        return cookie_baker;
    }
};
exports.add_farm = add_farm;
var add_mine = function (cookie_baker) {
    if (cookie_baker.number_of_cookie >= cookie_baker.mine_cost) {
        console.log("Enough cookie to buy a mine");
        // adding mine
        cookie_baker.number_of_mine = cookie_baker.number_of_mine + 1;
        // remove mine cost
        cookie_baker.number_of_cookie = cookie_baker.number_of_cookie - cookie_baker.mine_cost;
        // calculating next mine price
        cookie_baker.mine_cost = (0, exports.calculate_cost)(actions_1.action_type.increment_mine, cookie_baker);
        // calculate new cps;
        cookie_baker.mine_cps = cookie_baker.number_of_mine * exports.initial_mine_cps;
        return cookie_baker;
    }
    else {
        console.log("Not enough cookie to buy mine, need: " + cookie_baker.mine_cost + " actual amount: " + cookie_baker.number_of_cookie);
        return cookie_baker;
    }
};
exports.add_mine = add_mine;
