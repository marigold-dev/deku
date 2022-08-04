import { sendAsyncErr, sendAsyncRes, sendErr, sendJsonRes } from '@taqueria/node-sdk';
import { LoadedConfig, RequestArgs, SanitizedAbsPath } from '@taqueria/node-sdk/types';

// running external processes
import { exec as execRaw } from 'child_process';
import util from 'util';
// need to be wrapped in a promise
const exec = util.promisify(execRaw);


// TODO: What should this be, it was removed from the sdk
type PluginResponse =
	| void
	| {
		render: 'table';
		data: unknown[];
	};

interface Opts extends RequestArgs.ProxyRequestArgs {
	readonly path?: string;
	// TODO: change to the cli of deku
	readonly hash?: string;
}

const execute = async (opts: Opts): Promise<PluginResponse> => {
	const {
		task,
		path,
		hash,
		config,
	} = opts;

	/*const auth: PinataAuth = {
		// TODO: Where should this be stored?
		// pinataJwtToken: (config as Record<string, any>).credentials.pinataJwtToken,
		pinataJwtToken: process.env['pinataJwtToken'] as string,
	};

	if (!auth.pinataJwtToken) {
		throw new Error(`The 'credentials.pinataJwtToken' was not found in config`);
	}*/

	switch (task) {
		case 'deku-bootstrapper':
			// call externall process
			await exec(`deku-bootstrapper setup-identity`);
			return {
				render: 'table',
				data: [],
			};
		default:
			throw new Error(`${task} is not an understood task by the ipfs-pinata plugin`);
	}
};

export default async (args: RequestArgs.ProxyRequestArgs): Promise<PluginResponse> => {
	const opts = args as Opts;

	try {
		const resultRaw = await execute(opts) as Record<string, unknown>;
		// TODO: Fix deno parsing
		// Without this, `data.reduce is not a function`
		const result = ('data' in resultRaw) ? resultRaw.data : resultRaw;
		return sendJsonRes(result);
	} catch (err) {
		const error = err as Error;
		if (error.message) {
			return sendAsyncErr(error.message);
		}
	}
};
