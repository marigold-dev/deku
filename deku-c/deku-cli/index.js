"use strict";
exports.__esModule = true;
var Commander = require("commander");
var commands_1 = require("./commands");
var program = new Commander.Command();
(0, commands_1.make_generate_identity)(program);
(0, commands_1.make_originate_contract)(program);
(0, commands_1.make_show_storage)(program);
(0, commands_1.make_show_entrypoints)(program);
(0, commands_1.make_invoke)(program);
if (!process.argv.slice(2).length) {
    Commander.program.outputHelp();
}
program.parse(process.argv);
