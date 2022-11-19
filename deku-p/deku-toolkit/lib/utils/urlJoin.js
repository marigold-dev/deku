"use strict";
// https://github.com/jfromaniello/url-join
Object.defineProperty(exports, "__esModule", { value: true });
// MIT License
// Copyright (c) 2015 José F. Romaniello
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
function normalize(strArray) {
    const resultArray = [];
    if (strArray.length === 0) {
        return "";
    }
    if (typeof strArray[0] !== "string") {
        throw new TypeError("Url must be a string. Received " + strArray[0]);
    }
    // If the first part is a plain protocol, we combine it with the next part.
    if (strArray[0].match(/^[^/:]+:\/*$/) && strArray.length > 1) {
        strArray[0] = strArray.shift() + strArray[0];
    }
    // There must be two or three slashes in the file protocol, two slashes in anything else.
    if (strArray[0].match(/^file:\/\/\//)) {
        strArray[0] = strArray[0].replace(/^([^/:]+):\/*/, "$1:///");
    }
    else {
        strArray[0] = strArray[0].replace(/^([^/:]+):\/*/, "$1://");
    }
    for (let i = 0; i < strArray.length; i++) {
        let component = strArray[i];
        if (typeof component !== "string") {
            throw new TypeError("Url must be a string. Received " + component);
        }
        if (component === "") {
            continue;
        }
        if (i > 0) {
            // Removing the starting slashes for each component but the first.
            component = component.replace(/^[/]+/, "");
        }
        if (i < strArray.length - 1) {
            // Removing the ending slashes for each component but the last.
            component = component.replace(/[/]+$/, "");
        }
        else {
            // For the last component we will combine multiple slashes to a single one.
            component = component.replace(/[/]+$/, "/");
        }
        resultArray.push(component);
    }
    let str = resultArray.join("/");
    // Each input component is now separated by a single slash except the possible first plain protocol part.
    // remove trailing slash before parameters or hash
    str = str.replace(/\/(\?|&|#[^!])/g, "$1");
    // replace ? in parameters with &
    const parts = str.split("?");
    str = parts.shift() + (parts.length > 0 ? "?" : "") + parts.join("&");
    return str;
}
function urlJoin(...args) {
    const parts = Array.from(Array.isArray(args[0]) ? args[0] : args);
    return normalize(parts);
}
exports.default = urlJoin;
//# sourceMappingURL=urlJoin.js.map