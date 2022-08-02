"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.cookie_baker_arbitrary = void 0;
var fc = require("fast-check");
var state_1 = require("../src/state");
var cookie_baker_arbitrary = function () {
    return fc.tuple(fc.integer({ min: 0, max: 2000000000 }), fc.integer({ min: 0, max: 200 }), fc.integer({ min: 0, max: 200 }), fc.integer({ min: 0, max: 200 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 }), fc.integer({ min: 0, max: 10 })).map(function (_a) {
        var number_of_cookie = _a[0], number_of_cursor = _a[1], number_of_grandma = _a[2], number_of_farm = _a[3], number_of_mine = _a[4], number_of_free_cursor = _a[5], number_of_free_grandma = _a[6], number_of_free_farm = _a[7], number_of_free_mine = _a[8];
        var cookie_baker = (0, state_1.create_cookie_baker)(number_of_cookie, number_of_cursor, number_of_grandma, number_of_farm, number_of_mine, number_of_free_cursor, number_of_free_grandma, number_of_free_farm, number_of_free_mine);
        return cookie_baker;
    });
};
exports.cookie_baker_arbitrary = cookie_baker_arbitrary;
