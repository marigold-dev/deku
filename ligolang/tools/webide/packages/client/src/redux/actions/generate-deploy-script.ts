import { TezosToolkit } from '@taquito/taquito';
import { importKey } from '@taquito/signer';
import { Dispatch } from 'redux';
import slugify from 'slugify';

import {
  compileContract,
  compileStorage,
  getErrorMessage,
} from '../../services/api';
import { AppState } from '../app';
import { MichelsonFormat } from '../compile';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeOutputAction } from '../result';
import { CommandType } from '../types';
import { CancellableAction } from './cancellable';

const URL = 'https://api.tez.ie/keys/granadanet/';
const AUTHORIZATION_HEADER = 'Bearer ligo-ide';
const Tezos = new TezosToolkit('https://api.tez.ie/rpc/granadanet');

export async function fetchRandomPrivateKey(): Promise<string> {
  const response = await fetch(URL, {
    method: 'POST',
    headers: { Authorization: AUTHORIZATION_HEADER },
  });
  return response.text();
}

export class GenerateDeployScriptAction extends CancellableAction {
  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      dispatch({ ...new UpdateLoadingAction('Compiling contract...') });

      try {
        const { editor, generateDeployScript } = getState();

        const michelsonCodeJson = await compileContract(
          editor.language,
          editor.code,
          generateDeployScript.entrypoint,
          MichelsonFormat.Json
        );

        const michelsonCode = await compileContract(
          editor.language,
          editor.code,
          generateDeployScript.entrypoint
        );

        if (this.isCancelled()) {
          return;
        }

        dispatch({ ...new UpdateLoadingAction('Compiling storage...') });
        const michelsonStorageJson = await compileStorage(
          editor.language,
          editor.code,
          generateDeployScript.entrypoint,
          generateDeployScript.storage,
          MichelsonFormat.Json
        );

        const michelsonStorage = await compileStorage(
          editor.language,
          editor.code,
          generateDeployScript.entrypoint,
          generateDeployScript.storage
        );

        if (this.isCancelled()) {
          return;
        }

        dispatch({ ...new UpdateLoadingAction('Estimating burn cap...') });

        await importKey(Tezos, await fetchRandomPrivateKey());

        const estimate = await Tezos.estimate.originate({
          code: JSON.parse(michelsonCodeJson.result),
          init: JSON.parse(michelsonStorageJson.result),
        });

        if (this.isCancelled()) {
          return;
        }

        //const title = slugify(editor.title).toLowerCase() || 'untitled';
        const title = slugify(editor.title, {
          remove: /[*+~.()'"!]/g,
          lower: true,
        });
        const output = `tezos-client \\
  originate \\
  contract \\
  ${title} \\
  transferring 0 \\
  from $YOUR_SOURCE_ACCOUNT \\
  running '${michelsonCode.result.trim()}' \\
  --init '${michelsonStorage.result.trim()}' \\
  --burn-cap ${estimate.burnFeeMutez / 1000000}`;

        dispatch({
          ...new ChangeOutputAction(
            output,
            CommandType.GenerateDeployScript,
            false
          ),
        });
      } catch (ex) {
        if (this.isCancelled()) {
          return;
        }
        dispatch({
          ...new ChangeOutputAction(
            `Error: ${getErrorMessage(ex)}`,
            CommandType.GenerateDeployScript,
            true
          ),
        });
      }

      dispatch({ ...new DoneLoadingAction() });
    };
  }
}
