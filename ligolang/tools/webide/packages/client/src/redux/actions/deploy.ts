import { TezosToolkit } from '@taquito/taquito';
import { BeaconWallet } from '@taquito/beacon-wallet';
import { Dispatch } from 'redux';
import {
  NetworkType,
  BeaconEvent,
  defaultEventCallbacks,
} from '@airgap/beacon-sdk';

import {
  compileStorage,
  compileContract,
  deploy,
  getErrorMessage,
} from '../../services/api';
import { AppState } from '../app';
import { MichelsonFormat } from '../compile';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeContractAction, ChangeOutputAction } from '../result';
import { CommandType } from '../types';
import { CancellableAction } from './cancellable';

export class DeployAction extends CancellableAction {
  async deployOnServerSide(dispatch: Dispatch, getState: () => AppState) {
    const { editor: editorState, deploy: deployState } = getState();
    const network = deployState.network;
    dispatch({
      ...new UpdateLoadingAction(`Deploying to ${network} network...`),
    });

    return await deploy(
      editorState.language,
      editorState.code,
      deployState.entrypoint,
      deployState.storage,
      deployState.network
    );
  }

  requestBeaconPermissions = async (
    beaconWallet: any,
    launchNetwork: string
  ): Promise<void> => {
    if (launchNetwork === NetworkType.GRANADANET) {
      await beaconWallet.requestPermissions({
        network: {
          type: NetworkType.GRANADANET,
          name: 'Granadanet',
          rpcUrl: `https://api.tez.ie/rpc/granadanet`,
        },
      });
    } else if (launchNetwork === NetworkType.FLORENCENET) {
      await beaconWallet.requestPermissions({
        network: {
          type: NetworkType.FLORENCENET,
          name: 'Florencenet',
          rpcUrl: `https://api.tez.ie/rpc/florencenet`,
        },
      });
    } else if (launchNetwork === NetworkType.MAINNET) {
      await beaconWallet.requestPermissions({
        network: {
          type: NetworkType.MAINNET,
          name: 'Mainnet',
          rpcUrl: `https://api.tez.ie/rpc/mainnet`,
        },
      });
    }
  };

  async deployWithBeacon(dispatch: Dispatch, getState: () => AppState) {
    dispatch({ ...new UpdateLoadingAction('Compiling contract...') });

    const { editor: editorState, deploy: deployState } = getState();

    const michelsonStorage = await compileStorage(
      editorState.language,
      editorState.code,
      deployState.entrypoint,
      deployState.storage,
      MichelsonFormat.Json
    );

    const michelsonCode = await compileContract(
      editorState.language,
      editorState.code,
      deployState.entrypoint,
      MichelsonFormat.Json
    );

    let networkURL = 'https://api.tez.ie/rpc/florencenet';
    let network = { type: NetworkType.FLORENCENET };

    if (deployState.network === 'granadanet') {
      networkURL = 'https://api.tez.ie/rpc/granadanet';
      network = { type: NetworkType.GRANADANET };
    } else if (deployState.network === NetworkType.MAINNET) {
      networkURL = 'https://api.tez.ie/rpc/mainnet';
      network = { type: NetworkType.MAINNET };
    }

    const Tezos = new TezosToolkit(networkURL);
    const beaconWallet = new BeaconWallet({
      name: 'ligo-web-ide',
      preferredNetwork: network.type,
      eventHandlers: {
        [BeaconEvent.BROADCAST_REQUEST_SENT]: {
          handler: defaultEventCallbacks.BROADCAST_REQUEST_SENT,
        },
        // To enable your own wallet connection success message
        [BeaconEvent.PERMISSION_REQUEST_SUCCESS]: {
          // setting up the handler method will disable the default one
          handler: defaultEventCallbacks.PERMISSION_REQUEST_SUCCESS,
        },
        // to enable your own transaction sent message
        [BeaconEvent.OPERATION_REQUEST_SENT]: {
          handler: defaultEventCallbacks.OPERATION_REQUEST_SENT,
        },
        // to enable your own transaction success message
        [BeaconEvent.OPERATION_REQUEST_SUCCESS]: {
          // setting up the handler method will disable the default one
          handler: defaultEventCallbacks.OPERATION_REQUEST_SUCCESS,
        },
        [BeaconEvent.OPERATION_REQUEST_ERROR]: {
          // setting up the handler method will disable the default one
          handler: defaultEventCallbacks.OPERATION_REQUEST_ERROR,
        },
      },
    });
    await this.requestBeaconPermissions(beaconWallet, network.type);
    Tezos.setProvider({ wallet: beaconWallet });

    const walletOperation = await Tezos.wallet
      .originate({
        code: JSON.parse(michelsonCode.result),
        init: JSON.parse(michelsonStorage.result),
      })
      .send();

    if (this.isCancelled()) {
      return;
    }

    dispatch({
      ...new UpdateLoadingAction(`Deploying to ${network.type} network...`),
    });
    return {
      address: (await walletOperation.contract()).address,
      storage: michelsonStorage.result,
    };
  }

  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      const { deploy } = getState();
      try {
        const contract =
          deploy.signer === 'becon'
            ? await this.deployWithBeacon(dispatch, getState)
            : await this.deployOnServerSide(dispatch, getState);

        if (!contract || this.isCancelled()) {
          return;
        }

        dispatch({
          ...new ChangeContractAction(contract.address, CommandType.Deploy),
        });
        dispatch({
          ...new ChangeOutputAction(
            contract.storage,
            CommandType.Deploy,
            false
          ),
        });
      } catch (ex) {
        if (this.isCancelled()) {
          return;
        }
        dispatch({
          ...new ChangeContractAction('', CommandType.Deploy),
        });
        dispatch({
          ...new ChangeOutputAction(
            `Error: ${getErrorMessage(ex)}`,
            CommandType.Deploy,
            true
          ),
        });
      }

      dispatch({ ...new DoneLoadingAction() });
    };
  }
}
