import { DAppClient } from "@airgap/beacon-sdk";
import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import { fromBeaconSigner } from "@marigold-dev/deku-toolkit";
import { fromMemorySigner } from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";
import { Cmd } from "redux-loop";
import { makeJoinPayload, makePlayPayload, parseState } from "../core";
import contract from "../utils/contract";
import { wait } from "../utils";
import { Action } from "./reducer";
import { DEKU_RPC, IS_DEV, LIGO_RPC } from "../config";

// Just for dev purposes
const connectInMemory = async () => {
    const secrets = [
        "edsk4WVyqJ112qJREwC5TERn2AmmmNMyVhwWyaLgHosC48UFV6EnMG",
        "edsk4JaFU4PZ7vpp1kDyeE578zwJv2XpBP7QqhSQEw3mFS3rmBP5nJ",
        "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK",
        "edsk3Vjga1rxDHC9Bns7EN4CWkpaYoHwnHBPJMdERPtytxcx7A78PG",
        "edsk3tP2tJieBNsRTqaq8edcfekPxALp3W5F2Ef4tLecqf6XG66m5F",
        "edsk4Jkbb34LLESFeXdeNdwKhuaVarxT8QwRzerf5V9oqbjrjGAzNx",
        "edsk37WmmUhP2g5AN9Jz5FWgcspMyYbsqzrGW7bgUXMRrEd4M9aTaG",
        "edsk44pF6zGm8qV4YShrABDZDq5jrGydwrWmMiEwJWsJEarMSwCmB4",
        "edsk3MG557SuyCMyyjBa6n9MeZsLGRkHnXPpJedu8UrbUoEFh5q64K",
        "edsk33pagYM8CfcD2gokyE7RWD5bjcRAqdoYjLDuMohg5yT9r6UfYY",
    ];
    const secret = secrets[Math.floor(Math.random() * secrets.length)];
    const tezosSigner = new InMemorySigner(secret);
    const signer = fromMemorySigner(tezosSigner);
    const address = await signer.publicKeyHash();
    return { signer, address }
}

const connectBeaconWallet = async () => {
    const dAppClient = new DAppClient({ name: "Tic Tac Toe" });
    await dAppClient.requestPermissions();
    const signer = fromBeaconSigner(dAppClient);
    const address = await signer.publicKeyHash();
    return { signer, address };
}

export const connectCmd = (next: Action) => {
    const connect = IS_DEV ? connectInMemory : connectBeaconWallet;
    return Cmd.run(connect, {
        successActionCreator: ({ signer, address }) => {
            const deku = new DekuCClient({ dekuRpc: DEKU_RPC, ligoRpc: LIGO_RPC, signer });
            return { type: "CONNECTED", payload: { deku, address }, next }
        },
        failActionCreator: () => ({ type: "DISCONNECT" }),
    })
}

export const createGameCmd = (deku: DekuCClient, player: string) => {
    const initialStorage =
        `(Pair (Pair (Pair (Pair (Pair (Pair (Right Unit) (Right Unit)) (Right Unit) (Right Unit))(Pair (Right Unit) (Right Unit))(Right Unit) (Right Unit)) (Right Unit)) (Left "${player}")) "${player}" None)`;

    const createGame = async () => {
        const { operation, address } = await deku.originateContract({ kind: "jsligo", code: contract, initialStorage });
        await wait(DEKU_RPC, operation);
        return { address, operation };
    }

    return Cmd.run(createGame, {
        successActionCreator: ({ address }) => ({ type: "CREATE_GAME_SUCCESS", payload: { address } }),
        failActionCreator: () => ({ type: "CREATE_GAME_FAILURE" })
    });
}

export const fetchGameCmd = (contract: Contract, currentPlayer: string) => {
    const fetchGame = () => contract.getState()
        .then(state => parseState(state, currentPlayer))
        .then(state => {
            if (state === null) throw new Error("Contract not found")
            return state;
        });
    return Cmd.run(fetchGame, {
        successActionCreator: (state) => ({ type: "FETCH_GAME_SUCCESS", payload: state }),
        failActionCreator: () => ({ type: "FETCH_GAME_FAILURE" })
    })
}

export const playCmd = (contract: Contract, id: number) => {
    const play = () => {
        const move = makePlayPayload(id);
        return contract.invoke(move)
    }
    return Cmd.run(play, {
        successActionCreator: () => ({ type: "PLAY_SUCCESS" }),
        failActionCreator: () => ({ type: "PLAY_FAILURE" })
    })
}

export const joinCmd = (contract: Contract, address: string) => {
    const join = () => {
        const joinPayload = makeJoinPayload();
        return contract.invoke(joinPayload);
    };
    return Cmd.run(join, {
        successActionCreator: () => ({ type: "JOIN_SUCCESS", payload: { address } }),
        failActionCreator: () => ({ type: "JOIN_FAILURE" }),
    })
}