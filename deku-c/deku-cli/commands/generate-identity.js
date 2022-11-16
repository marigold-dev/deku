"use strict";
// FIXME: copied from deku-p/deku-cli
exports.__esModule = true;
var wallet_1 = require("../core/wallet");
function make(command) {
    var subcommand = command.command("generate-identity");
    subcommand
        .option("-o, --output <path>", "JSON wallet output")
        .action(function (args) {
        (0, wallet_1.generate)(args.output === undefined).then(function (wallet) {
            if (args.output !== undefined) {
                (0, wallet_1.save)(wallet, args.output);
            }
        });
    });
    return command;
}
exports["default"] = make;
