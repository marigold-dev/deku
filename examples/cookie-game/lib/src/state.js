"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.addFarm = exports.addGrandma = exports.addCursor = exports.addCookie = exports.calculateCost = exports.createCookieBaker = exports.initial_farmCost = exports.initial_grandmaCost = exports.initial_cursorCost = exports.initial_farmCps = exports.initial_grandmaCps = exports.initial_cursor_cps = void 0;
var actions_1 = require("./actions");
exports.initial_cursor_cps = 0.1;
exports.initial_grandmaCps = 1;
exports.initial_farmCps = 8;
exports.initial_cursorCost = 15;
exports.initial_grandmaCost = 100;
exports.initial_farmCost = 1100;
var createCookieBaker = function (numberOfCookie, numberOfCursor, numberOfGrandma, numberOfFarm, numberOfFreeCursor, numberOfFreeGrandma, numberOfFreeFarm) {
    var cookie_baker = {
        numberOfCookie: numberOfCookie,
        numberOfCursor: numberOfCursor,
        numberOfGrandma: numberOfGrandma,
        numberOfFarm: numberOfFarm,
        numberOfFreeCursor: numberOfFreeCursor,
        numberOfFreeGrandma: numberOfFreeGrandma,
        numberOfFreeFarm: numberOfFreeFarm,
        cursorCost: 0,
        grandmaCost: 0,
        farmCost: 0,
        cursorCps: 0,
        grandmaCps: 0,
        farmCps: 0,
    };
    var cursorCost = (0, exports.calculateCost)(actions_1.actions.incrementCursor, cookie_baker);
    var grandmaCost = (0, exports.calculateCost)(actions_1.actions.incrementGrandma, cookie_baker);
    var farmCost = (0, exports.calculateCost)(actions_1.actions.incrementFarm, cookie_baker);
    var cursor_cps = cookie_baker.numberOfCursor * exports.initial_cursor_cps;
    var grandmaCps = cookie_baker.numberOfGrandma * exports.initial_grandmaCps;
    var farmCps = cookie_baker.numberOfFarm * exports.initial_farmCps;
    cookie_baker.cursorCost = cursorCost;
    cookie_baker.grandmaCost = grandmaCost;
    cookie_baker.farmCost = farmCost;
    cookie_baker.cursorCps = cursor_cps;
    cookie_baker.grandmaCps = grandmaCps;
    cookie_baker.farmCps = farmCps;
    return cookie_baker;
};
exports.createCookieBaker = createCookieBaker;
var calculateCost = function (action, cookie_baker) {
    switch (action) {
        case actions_1.actions.incrementCookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case actions_1.actions.incrementCursor:
            console.log("Calculating price for next cursor, actual price is: " + cookie_baker.cursorCost);
            var new_cursor_price = Math.floor(exports.initial_cursorCost * Math.pow(1.15, cookie_baker.numberOfCursor - cookie_baker.numberOfFreeCursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;
        case actions_1.actions.incrementGrandma:
            console.log("Calculating price for next grandma, actual price is: " + cookie_baker.grandmaCost);
            var new_grandma_price = Math.floor(exports.initial_grandmaCost * Math.pow(1.15, cookie_baker.numberOfGrandma - cookie_baker.numberOfFreeGrandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;
        case actions_1.actions.incrementFarm:
            console.log("Calculating price for next farm, actual price is: " + cookie_baker.farmCost);
            var new_farm_price = Math.floor(exports.initial_farmCost * Math.pow(1.15, cookie_baker.numberOfFarm - cookie_baker.numberOfFreeFarm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;
    }
};
exports.calculateCost = calculateCost;
var addCookie = function (cookie_baker) {
    console.log("Adding cookie: " + cookie_baker.numberOfCookie);
    cookie_baker.numberOfCookie = cookie_baker.numberOfCookie + 1;
    console.log("Successfully added cookie: " + cookie_baker.numberOfCookie);
    return cookie_baker;
};
exports.addCookie = addCookie;
var addCursor = function (cookie_baker) {
    if (cookie_baker.numberOfCookie >= cookie_baker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookie_baker.numberOfCursor = cookie_baker.numberOfCursor + 1;
        // removing cursor cost
        cookie_baker.numberOfCookie = cookie_baker.numberOfCookie - cookie_baker.cursorCost;
        // calculating next cursor price
        cookie_baker.cursorCost = (0, exports.calculateCost)(actions_1.actions.incrementCursor, cookie_baker);
        // calculate new cps
        cookie_baker.cursorCps = cookie_baker.numberOfCursor * exports.initial_cursor_cps;
        return cookie_baker;
    }
    else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookie_baker.cursorCps + " actual amount: " + cookie_baker.numberOfCookie);
        return cookie_baker;
    }
};
exports.addCursor = addCursor;
var addGrandma = function (cookie_baker) {
    if (cookie_baker.numberOfCookie >= cookie_baker.grandmaCost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookie_baker.numberOfGrandma = cookie_baker.numberOfGrandma + 1;
        // removing grandma cost
        cookie_baker.numberOfCookie = cookie_baker.numberOfCookie - cookie_baker.grandmaCost;
        // calculating next grandma price
        cookie_baker.grandmaCost = (0, exports.calculateCost)(actions_1.actions.incrementGrandma, cookie_baker);
        // calculate new cps
        cookie_baker.grandmaCps = cookie_baker.numberOfGrandma * exports.initial_grandmaCps;
        return cookie_baker;
    }
    else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookie_baker.grandmaCost + " actual amount: " + cookie_baker.numberOfCookie);
        return cookie_baker;
    }
};
exports.addGrandma = addGrandma;
var addFarm = function (cookie_baker) {
    if (cookie_baker.numberOfCookie >= cookie_baker.farmCost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookie_baker.numberOfFarm = cookie_baker.numberOfFarm + 1;
        // removing farm cost
        cookie_baker.numberOfCookie = cookie_baker.numberOfCookie - cookie_baker.farmCost;
        // calculating next farm price
        cookie_baker.farmCost = (0, exports.calculateCost)(actions_1.actions.incrementFarm, cookie_baker);
        // calculate new cps
        cookie_baker.farmCps = cookie_baker.numberOfFarm * exports.initial_farmCps;
        return cookie_baker;
    }
    else {
        console.log("Not enough cookie to buy a farm, needed: " + cookie_baker.farmCost + " actual amount: " + cookie_baker.numberOfCookie);
        return cookie_baker;
    }
};
exports.addFarm = addFarm;
