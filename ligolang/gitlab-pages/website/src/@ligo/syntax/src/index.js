const path = require('path');

module.exports = function(context, options) {
    return {
        name: 'ligo-syntax-plugin',

        getThemePath() {
            return path.resolve(__dirname, './theme');
        }
    }
}