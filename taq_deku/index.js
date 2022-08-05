import {Plugin as $8CNkB$Plugin, Task as $8CNkB$Task, sendJsonRes as $8CNkB$sendJsonRes, sendAsyncErr as $8CNkB$sendAsyncErr} from "@taqueria/node-sdk";
import {exec as $8CNkB$exec} from "child_process";
import $8CNkB$util from "util";





// need to be wrapped in a promise
const $b297f5d0aa12bc82$var$exec = (0, $8CNkB$util).promisify((0, $8CNkB$exec));
const $b297f5d0aa12bc82$var$execute = async (opts)=>{
    const { task: task , path: path , hash: hash , config: config ,  } = opts;
    /*const auth: PinataAuth = {
		// TODO: Where should this be stored?
		// pinataJwtToken: (config as Record<string, any>).credentials.pinataJwtToken,
		pinataJwtToken: process.env['pinataJwtToken'] as string,
	};

	if (!auth.pinataJwtToken) {
		throw new Error(`The 'credentials.pinataJwtToken' was not found in config`);
	}*/ switch(task){
        case "deku-bootstrapper":
            // call externall process
            await $b297f5d0aa12bc82$var$exec(`deku-bootstrapper setup-identity`);
            return {
                render: "table",
                data: []
            };
        default:
            throw new Error(`${task} is not an understood task by the ipfs-pinata plugin`);
    }
};
var $b297f5d0aa12bc82$export$2e2bcd8739ae039 = async (args)=>{
    const opts = args;
    try {
        const resultRaw = await $b297f5d0aa12bc82$var$execute(opts);
        // TODO: Fix deno parsing
        // Without this, `data.reduce is not a function`
        const result = "data" in resultRaw ? resultRaw.data : resultRaw;
        return (0, $8CNkB$sendJsonRes)(result);
    } catch (err) {
        const error = err;
        if (error.message) return (0, $8CNkB$sendAsyncErr)(error.message);
    }
};


(0, $8CNkB$Plugin).create(()=>({
        schema: "0.1",
        version: "0.4.0",
        alias: "deku",
        tasks: [
            (0, $8CNkB$Task).create({
                task: "deku-bootstrapper",
                command: "deku-bootstrapper",
                description: "TODO:Upload and pin files using your pinata account.",
                aliases: [],
                handler: "proxy",
                encoding: "json"
            }), 
        ],
        proxy: $b297f5d0aa12bc82$export$2e2bcd8739ae039
    }), process.argv);


//# sourceMappingURL=index.js.map
