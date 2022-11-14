import { DAppClient } from "@airgap/beacon-sdk";
import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import { fromBeaconSigner } from "@marigold-dev/deku-toolkit";
import { fromMemorySigner } from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";
import { Cmd } from "redux-loop";
import { makePlayPayload, parseState } from "../core";
import contract from "../utils/contract";
import { wait } from "../utils";
import { Action } from "./reducer";

const connectInMemory = async () => {
    const secrets = [
        "edsk4Jex4ueokTLnc7xnkdXBZPfo1MQNeNPeDEmbjo7Wk2DSdVwjJD",
        "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
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
    const connect = connectInMemory // Change this when deploying
    return Cmd.run(connect, {
        successActionCreator: ({ signer, address }) => {
            const dekuRpc = "http://localhost:8080"; // TODO: change this url 
            const ligoRpc = "http://localhost:9090"; // TODO: change this url
            const deku = new DekuCClient({ dekuRpc, ligoRpc, signer });
            return { type: "CONNECTED", payload: { deku, address }, next }
        },
        failActionCreator: () => ({ type: "DISCONNECT" }),
    })
}

export const createGameCmd = (deku: DekuCClient, player: string) => {
    const initialStorage =
        `(Pair(Pair(Pair(Pair(Pair(Pair(Left(Right Unit))(Right Unit))(Left(Left Unit))(Right Unit))(Pair(Right Unit)(Right Unit))(Right Unit)(Right Unit))(Right Unit))(Left "${player}")) "${player}" "${player}")`;

    const createGame = async () => {
        const { operation, address } = await deku.originateContract({ kind: "jsligo", code: contract, initialStorage });
        await wait("http://localhost:8080", operation); // TODO: inject this in createGameCmd signature
        // await new Promise(resolve => setTimeout(resolve, 3000));
        // return {address : "dev address", operation}
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
    const join = () => Promise.resolve();
    return Cmd.run(join, {
        successActionCreator: () => ({ type: "JOIN_SUCCESS", payload: { address } }),
        failActionCreator: () => ({ type: "JOIN_FAILURE" }),
    })
}