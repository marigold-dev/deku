"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Cookie_baker = void 0;
var actions_1 = require("./actions");
var initial_cursor_cps = 0.1;
var initial_grandma_cps = 1;
var initial_farm_cps = 8;
var initial_cursor_cost = 15;
var initial_grandma_cost = 100;
var initial_farm_cost = 1100;
var Cookie_baker = /** @class */ (function () {
    function Cookie_baker(number_of_cookie, number_of_cursor, number_of_grandma, number_of_farm, number_of_free_cursor, number_of_free_grandma, number_of_free_farm) {
        console.log("creating baker with: " + number_of_cookie + " cookies");
        if (number_of_cookie != undefined) {
            this.number_of_cookie = number_of_cookie;
            this.number_of_cursor = number_of_cursor;
            this.number_of_grandma = number_of_grandma;
            this.number_of_farm = number_of_farm;
            this.number_of_free_cursor = number_of_free_cursor;
            this.number_of_free_grandma = number_of_free_grandma;
            this.number_of_free_farm = number_of_free_farm;
        }
        else {
            console.log("Creating a new coookie_baker");
            this.number_of_cookie = 0;
            this.number_of_cursor = 0;
            this.number_of_grandma = 0;
            this.number_of_farm = 0;
            this.number_of_free_cursor = 0;
            this.number_of_free_grandma = 0;
            this.number_of_free_farm = 0;
        }
        this.cursor_cost = initial_cursor_cost;
        this.grandma_cost = initial_grandma_cost;
        this.farm_cost = initial_farm_cost;
        this.cursor_cps = initial_cursor_cps;
        this.grandma_cps = initial_grandma_cps;
        this.farm_cps = initial_farm_cps;
    }
    Object.defineProperty(Cookie_baker.prototype, "get_number_of_cookie", {
        /**
         * Getter number_of_cookie
         * @return {number}
         */
        get: function () {
            return this.number_of_cookie;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_number_of_cursor", {
        /**
         * Getter number_of_cursor
         * @return {number}
         */
        get: function () {
            return this.number_of_cursor;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_number_of_grandma", {
        /**
         * Getter number_of_grandma
         * @return {number}
         */
        get: function () {
            return this.number_of_grandma;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_number_of_farm", {
        /**
         * Getter number_of_farm
         * @return {number}
         */
        get: function () {
            return this.number_of_farm;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_number_of_free_cursor", {
        /**
         * Getter number_of_free_cursor
         * @return {number}
         */
        get: function () {
            return this.number_of_free_cursor;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_number_of_free_grandma", {
        /**
         * Getter number_of_free_grandma
         * @return {number}
         */
        get: function () {
            return this.number_of_free_grandma;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_number_of_free_farm", {
        /**
         * Getter number_of_free_farm
         * @return {number}
         */
        get: function () {
            return this.number_of_free_farm;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_cursor_cost", {
        /**
         * Getter cursor_cost
         * @return {number}
         */
        get: function () {
            return this.cursor_cost;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_grandma_cost", {
        /**
         * Getter grandma_cost
         * @return {number}
         */
        get: function () {
            return this.grandma_cost;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_farm_cost", {
        /**
         * Getter farm_cost
         * @return {number}
         */
        get: function () {
            return this.farm_cost;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_cursor_cps", {
        /**
         * Getter cursor_cps
         * @return {number}
         */
        get: function () {
            return this.cursor_cps;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_grandma_cps", {
        /**
         * Getter grandma_cps
         * @return {number}
         */
        get: function () {
            return this.grandma_cps;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "get_farm_cps", {
        /**
         * Getter farm_cps
         * @return {number}
         */
        get: function () {
            return this.farm_cps;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_number_of_cookie", {
        /**
         * Setter number_of_cookie
         * @param {number} value
         */
        set: function (value) {
            this.number_of_cookie = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_number_of_cursor", {
        /**
         * Setter number_of_cursor
         * @param {number} value
         */
        set: function (value) {
            this.number_of_cursor = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_number_of_grandma", {
        /**
         * Setter number_of_grandma
         * @param {number} value
         */
        set: function (value) {
            this.number_of_grandma = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_number_of_farm", {
        /**
         * Setter number_of_farm
         * @param {number} value
         */
        set: function (value) {
            this.number_of_farm = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_number_of_free_cursor", {
        /**
         * Setter number_of_free_cursor
         * @param {number} value
         */
        set: function (value) {
            this.number_of_free_cursor = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_number_of_free_grandma", {
        /**
         * Setter number_of_free_grandma
         * @param {number} value
         */
        set: function (value) {
            this.number_of_free_grandma = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_number_of_free_farm", {
        /**
         * Setter number_of_free_farm
         * @param {number} value
         */
        set: function (value) {
            this.number_of_free_farm = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_cursor_cost", {
        /**
         * Setter cursor_cost
         * @param {number} value
         */
        set: function (value) {
            this.cursor_cost = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_grandma_cost", {
        /**
         * Setter grandma_cost
         * @param {number} value
         */
        set: function (value) {
            this.grandma_cost = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_farm_cost", {
        /**
         * Setter farm_cost
         * @param {number} value
         */
        set: function (value) {
            this.farm_cost = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_cursor_cps", {
        /**
         * Setter cursor_cps
         * @param {number} value
         */
        set: function (value) {
            this.cursor_cps = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_grandma_cps", {
        /**
         * Setter grandma_cps
         * @param {number} value
         */
        set: function (value) {
            this.grandma_cps = value;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(Cookie_baker.prototype, "set_farm_cps", {
        /**
         * Setter farm_cps
         * @param {number} value
         */
        set: function (value) {
            this.farm_cps = value;
        },
        enumerable: false,
        configurable: true
    });
    Cookie_baker.prototype.add_cookie = function () {
        console.log("Adding cookie: " + this.get_number_of_cookie);
        this.set_number_of_cookie = this.get_number_of_cookie + 1;
        console.log("Successfully added cookie: " + this.get_number_of_cookie);
    };
    Cookie_baker.prototype.add_cursor = function () {
        if (this.get_number_of_cookie >= this.get_cursor_cost) {
            console.log("Enough cookie to buy a cursor");
            // adding cursor
            this.set_number_of_cursor = this.get_number_of_cursor + 1;
            // removing cursor cost
            this.set_number_of_cookie = this.get_number_of_cookie - this.get_cursor_cost;
            // calculating next cursor price
            this.set_cursor_cost = this.calculate_cost(actions_1.action_type.increment_cursor);
            // calculate new cps
            this.set_cursor_cps = this.get_number_of_cursor * initial_cursor_cps;
        }
        else {
            console.log("Not enough cookie to buy a cursor, needed: " + this.get_cursor_cost + " actual amount: " + this.get_number_of_cookie);
        }
    };
    Cookie_baker.prototype.add_grandma = function () {
        if (this.get_number_of_cookie >= this.get_grandma_cost) {
            console.log("Enough cookie to buy a grandma");
            // adding grandma
            this.set_number_of_grandma = this.get_number_of_grandma + 1;
            // removing grandma cost
            this.set_number_of_cookie = this.get_number_of_cookie - this.get_grandma_cost;
            // calculating next grandma price
            this.set_grandma_cost = this.calculate_cost(actions_1.action_type.increment_grandma);
            // calculate new cps
            this.set_grandma_cps = this.get_number_of_grandma * initial_grandma_cps;
        }
        else {
            console.log("Not enough cookie to buy a grandma, needed: " + this.get_grandma_cost + " actual amount: " + this.get_number_of_cookie);
        }
    };
    Cookie_baker.prototype.add_farm = function () {
        if (this.get_number_of_cookie >= this.get_farm_cost) {
            console.log("Enough cookie to buy a farm");
            // adding farm
            this.set_number_of_farm = this.get_number_of_farm + 1;
            // removing farm cost
            this.set_number_of_cookie = this.get_number_of_cookie - this.get_farm_cost;
            // calculating next farm price
            this.set_farm_cost = this.calculate_cost(actions_1.action_type.increment_farm);
            // calculate new cps
            this.set_farm_cps = this.get_number_of_farm * initial_farm_cps;
        }
        else {
            console.log("Not enough cookie to buy a farm, needed: " + this.get_farm_cost + " actual amount: " + this.get_number_of_cookie);
        }
    };
    Cookie_baker.prototype.calculate_cost = function (action) {
        switch (action) {
            case actions_1.action_type.increment_cookie:
                console.log("Cookie does not have cost");
                throw new Error("Cookie does not have cost");
            case actions_1.action_type.increment_cursor:
                console.log("Calculating price for next cursor, actual price is: " + this.get_cursor_cost);
                var new_cursor_price = Math.floor(initial_cursor_cost * Math.pow(1.15, this.get_number_of_cursor - this.get_number_of_free_cursor));
                console.log("New cursor price is: " + new_cursor_price);
                return new_cursor_price;
            case actions_1.action_type.increment_grandma:
                console.log("Calculating price for next grandma, actual price is: " + this.get_grandma_cost);
                var new_grandma_price = Math.floor(initial_grandma_cost * Math.pow(1.15, this.get_number_of_grandma - this.get_number_of_free_grandma));
                console.log("New grandma price is: " + new_grandma_price);
                return new_grandma_price;
            case actions_1.action_type.increment_farm:
                console.log("Calculating price for next farm, actual price is: " + this.get_farm_cost);
                var new_farm_price = Math.floor(initial_farm_cost * Math.pow(1.15, this.get_number_of_farm - this.get_number_of_free_farm));
                console.log("New farm price is: " + new_farm_price);
                return new_farm_price;
        }
    };
    return Cookie_baker;
}());
exports.Cookie_baker = Cookie_baker;
