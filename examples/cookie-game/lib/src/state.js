"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.addFactory = exports.addMine = exports.addFarm = exports.addGrandma = exports.addCursor = exports.addCookie = exports.calculateCost = exports.createCookieBaker = exports.initial_factoryCost = exports.initial_mineCost = exports.initial_farmCost = exports.initial_grandmaCost = exports.initial_cursorCost = exports.initial_factoryCps = exports.initial_mineCps = exports.initial_farmCps = exports.initial_grandmaCps = exports.initial_cursorCps = void 0;
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
exports.initial_cursorCps = 0.1;
exports.initial_grandmaCps = 1;
exports.initial_farmCps = 8;
exports.initial_mineCps = 47;
exports.initial_factoryCps = 260;
exports.initial_cursorCost = 15;
exports.initial_grandmaCost = 100;
exports.initial_farmCost = 1100;
exports.initial_mineCost = 12000;
exports.initial_factoryCost = 130000;
var createCookieBaker = function (numberOfCookie, numberOfCursor, numberOfGrandma, numberOfFarm, numberOfMine, numberOfFactory, numberOfFreeCursor, numberOfFreeGrandma, numberOfFreeFarm, numberOfFreeMine, numberOfFreeFactory) {
    var cookieBaker = {
        numberOfCookie: numberOfCookie,
        numberOfCursor: numberOfCursor,
        numberOfGrandma: numberOfGrandma,
        numberOfFarm: numberOfFarm,
        numberOfMine: numberOfMine,
        numberOfFactory: numberOfFactory,
        numberOfFreeCursor: numberOfFreeCursor,
        numberOfFreeGrandma: numberOfFreeGrandma,
        numberOfFreeFarm: numberOfFreeFarm,
        numberOfFreeMine: numberOfFreeMine,
        numberOfFreeFactory: numberOfFreeFactory,
        cursorCost: 0,
        grandmaCost: 0,
        farmCost: 0,
        mineCost: 0,
        factoryCost: 0,
        cursorCps: 0,
        grandmaCps: 0,
        farmCps: 0,
        mineCps: 0,
        factoryCps: 0,
    };
    var cursorCost = (0, exports.calculateCost)(actions_1.actions.incrementCursor, cookieBaker);
    var grandmaCost = (0, exports.calculateCost)(actions_1.actions.incrementGrandma, cookieBaker);
    var farmCost = (0, exports.calculateCost)(actions_1.actions.incrementFarm, cookieBaker);
    var mineCost = (0, exports.calculateCost)(actions_1.actions.incrementMine, cookieBaker);
    var factoryCost = (0, exports.calculateCost)(actions_1.actions.incrementFactory, cookieBaker);
    var cursorCps = cookieBaker.numberOfCursor * exports.initial_cursorCps;
    var grandmaCps = cookieBaker.numberOfGrandma * exports.initial_grandmaCps;
    var farmCps = cookieBaker.numberOfFarm * exports.initial_farmCps;
    var mineCps = cookieBaker.numberOfMine * exports.initial_mineCps;
    var factoryCps = cookieBaker.numberOfFactory * exports.initial_factoryCps;
    cookieBaker.cursorCost = cursorCost;
    cookieBaker.grandmaCost = grandmaCost;
    cookieBaker.farmCost = farmCost;
    cookieBaker.mineCost = mineCost;
    cookieBaker.factoryCost = factoryCost;
    cookieBaker.cursorCps = cursorCps;
    cookieBaker.grandmaCps = grandmaCps;
    cookieBaker.farmCps = farmCps;
    cookieBaker.mineCps = mineCps;
    cookieBaker.factoryCps = factoryCps;
    return cookieBaker;
};
exports.createCookieBaker = createCookieBaker;
var calculateCost = function (action, cookieBaker) {
    switch (action) {
        case actions_1.actions.incrementCookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case actions_1.actions.incrementCursor:
            console.log("Calculating price for next cursor, actual price is: " + cookieBaker.cursorCost);
            var new_cursor_price = Math.floor(exports.initial_cursorCost * Math.pow(1.15, cookieBaker.numberOfCursor - cookieBaker.numberOfFreeCursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;
        case actions_1.actions.incrementGrandma:
            console.log("Calculating price for next grandma, actual price is: " + cookieBaker.grandmaCost);
            var new_grandma_price = Math.floor(exports.initial_grandmaCost * Math.pow(1.15, cookieBaker.numberOfGrandma - cookieBaker.numberOfFreeGrandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;
        case actions_1.actions.incrementFarm:
            console.log("Calculating price for next farm, actual price is: " + cookieBaker.farmCost);
            var new_farm_price = Math.floor(exports.initial_farmCost * Math.pow(1.15, cookieBaker.numberOfFarm - cookieBaker.numberOfFreeFarm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;
        case actions_1.actions.incrementMine:
            console.log("Calculating price for next mine, actual price is: " + cookieBaker.mineCost);
            var new_mine_price = Math.floor(exports.initial_mineCost * Math.pow(1.15, cookieBaker.numberOfMine - cookieBaker.numberOfFreeMine));
            console.log("New mine price is: " + new_mine_price);
            return new_mine_price;
        case actions_1.actions.incrementFactory:
            console.log("Calculating price for next factory, actual price is: " + cookieBaker.factoryCost);
            var new_factory_price = Math.floor(exports.initial_factoryCost * Math.pow(1.15, cookieBaker.numberOfFactory - cookieBaker.numberOfFreeFactory));
            console.log("New factory price is: " + new_factory_price);
            return new_factory_price;
    }
};
exports.calculateCost = calculateCost;
var addCookie = function (cookieBaker) {
    console.log("Adding cookie: " + cookieBaker.numberOfCookie);
    cookieBaker.numberOfCookie = cookieBaker.numberOfCookie + 1;
    console.log("Successfully added cookie: " + cookieBaker.numberOfCookie);
    return cookieBaker;
};
exports.addCookie = addCookie;
var addCursor = function (cookieBaker) {
    if (cookieBaker.numberOfCookie >= cookieBaker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookieBaker.numberOfCursor = cookieBaker.numberOfCursor + 1;
        // removing cursor cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.cursorCost;
        // calculating next cursor price
        cookieBaker.cursorCost = (0, exports.calculateCost)(actions_1.actions.incrementCursor, cookieBaker);
        // calculate new cps
        cookieBaker.cursorCps = cookieBaker.numberOfCursor * exports.initial_cursorCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookieBaker.cursorCps + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
};
exports.addCursor = addCursor;
var addGrandma = function (cookieBaker) {
    if (cookieBaker.numberOfCookie >= cookieBaker.grandmaCost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookieBaker.numberOfGrandma = cookieBaker.numberOfGrandma + 1;
        // removing grandma cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.grandmaCost;
        // calculating next grandma price
        cookieBaker.grandmaCost = (0, exports.calculateCost)(actions_1.actions.incrementGrandma, cookieBaker);
        // calculate new cps
        cookieBaker.grandmaCps = cookieBaker.numberOfGrandma * exports.initial_grandmaCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookieBaker.grandmaCost + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
};
exports.addGrandma = addGrandma;
var addFarm = function (cookieBaker) {
    if (cookieBaker.numberOfCookie >= cookieBaker.farmCost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookieBaker.numberOfFarm = cookieBaker.numberOfFarm + 1;
        // removing farm cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.farmCost;
        // calculating next farm price
        cookieBaker.farmCost = (0, exports.calculateCost)(actions_1.actions.incrementFarm, cookieBaker);
        // calculate new cps
        cookieBaker.farmCps = cookieBaker.numberOfFarm * exports.initial_farmCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a farm, needed: " + cookieBaker.farmCost + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
};
exports.addFarm = addFarm;
var addMine = function (cookieBaker) {
    if (cookieBaker.numberOfCookie >= cookieBaker.mineCost) {
        console.log("Enough cookie to buy a mine");
        // adding mine
        cookieBaker.numberOfMine = cookieBaker.numberOfMine + 1;
        // removing mine cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.mineCost;
        // calculating next mine price
        cookieBaker.mineCost = (0, exports.calculateCost)(actions_1.actions.incrementMine, cookieBaker);
        // calculate new cps
        cookieBaker.mineCps = cookieBaker.numberOfMine * exports.initial_mineCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a mine, needed: " + cookieBaker.mineCost + " actual amount: " + cookieBaker.numberOfMine);
        return cookieBaker;
    }
};
exports.addMine = addMine;
var addFactory = function (cookieBaker) {
    if (cookieBaker.numberOfCookie >= cookieBaker.factoryCost) {
        console.log("Enough cookie to buy a factory");
        // adding factory
        cookieBaker.numberOfFactory = cookieBaker.numberOfFactory + 1;
        // removing factory cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.factoryCost;
        // calculating next factory price
        cookieBaker.factoryCost = (0, exports.calculateCost)(actions_1.actions.incrementFactory, cookieBaker);
        // calculate new cps
        cookieBaker.factoryCps = cookieBaker.numberOfFactory * exports.initial_factoryCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a factory, needed: " + cookieBaker.factoryCost + " actual amount: " + cookieBaker.numberOfFactory);
        return cookieBaker;
    }
};
exports.addFactory = addFactory;
