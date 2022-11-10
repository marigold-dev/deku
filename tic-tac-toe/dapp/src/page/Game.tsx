import { DekuSigner } from "@marigold-dev/deku-toolkit/lib/utils/signers";
import React, { useEffect, useState } from "react";
import Connect from "../components/Connect";
import { fromBeaconSigner, fromMemorySigner } from '@marigold-dev/deku-toolkit';
import { InMemorySigner } from '@taquito/signer';
import { DAppClient } from "@airgap/beacon-sdk";
import Connected from "../components/Connected";
import { DekuCClient } from "@marigold-dev/deku-c-toolkit";
import contract from "../utils/contract";
import { wait } from "../utils";
import Board from '../components/Board';
import { makeInitStorage, parseState, play, State } from "../core";

const dekuRpc = "http://0.0.0.0:8080";
const ligoRpc = "http://0.0.0.0:9090";
const dAppUri = "http://localhost:3000"

const reset = (err: any) => {
    console.error(err);
    window.location.search = ""; //reload the page
}

interface user {
    address: string;
    signer: DekuSigner
}

interface Step1Property {
    connect: () => void,
}

const Step1 = ({ connect }: Step1Property) => {
    return (
        <div className="App">
            <Connect connect={connect} />
        </div>
    );
}

interface Step2Property {
    deku: DekuCClient,
    user: user,
    disconnect: () => void,
    setGameAddress: (addr: string) => void,
}

const Step2 = ({ deku, user, disconnect, setGameAddress }: Step2Property) => {
    const [opponent, setOpponent] = useState("");
    const initialStorage = makeInitStorage(user.address, opponent);


    // check if a contract is in the url
    const queryString = window.location.search;
    const urlParams = new URLSearchParams(queryString);
    const address = urlParams.get('address');
    useEffect(() => {
        if (address !== null) {
            setGameAddress(address);
        }
    }, [address, setGameAddress]);

    const originate = () => {
        if (opponent) {
            deku.originateContract({ kind: "jsligo", code: contract, initialStorage })
                .then(async ({ address, operation }) => {
                    await wait(dekuRpc, operation);
                    setGameAddress(address);
                })
                .catch(reset)
        }
    }

    if (address !== null) {
        return (
            <div className="App">
                <Connected address={user.address} disconnect={disconnect} />
            </div>
        );
    }

    return (
        <div className="App">
            <Connected address={user.address} disconnect={disconnect} />
            <input value={opponent} onChange={event => setOpponent(event.target.value)} />
            <div id="start-game" className={opponent ? "start-enable" : "start-disable"} onClick={originate}>Start a game</div> {/* should be replace by button ?? */}
        </div>
    );
}

interface Step3Property {
    deku: DekuCClient,
    user: user,
    gameAddress: string,
    disconnect: () => void,
}

const Step3 = ({ deku, user, gameAddress, disconnect }: Step3Property) => {
    const [state, setState] = useState<State | null>(null);

    useEffect(() => {
        const contract = deku.contract(gameAddress);
        contract.getState()
            .then(state => {
                if (state === null) { throw new Error("Contract does not exist") };
                return parseState(state)
            })
            .then(setState)
            .catch(reset);
    }, [deku, gameAddress]);


    useEffect(() => {
        if (!gameAddress) return;
        const contract = deku.contract(gameAddress);
        contract.onNewState(state => {
            const parsed = parseState(state);
            setState(parsed);
        })
    }, [deku, gameAddress]);

    useEffect(() => {
    }, [state?.gameState])

    const onClick = ({ id }: { id: number }) => {
        const move = play(id);
        deku.contract(gameAddress)
            .invoke(move)
            .then(console.log)
            .catch(console.error)
    }

    const uri = `${dAppUri}?address=${gameAddress}`;
    const player =
        state?.gameState.type === "PlayerTurn"
            ? (state?.gameState.payload === user.address ? "It's your turn to play" : "Waiting for your oponent")
            : state?.gameState.type === "Winner"
                ? (state?.gameState.payload === user.address ? "You won the game !! GG" : ":grimace:")
                : "Waiting";

    return (
        <div className="App">
            <Connected address={user.address} disconnect={disconnect} />
            <div id="link">{uri}</div>
            <div id="state">{player}</div>
            {state ? <Board cells={state.game} onCellClick={onClick} /> : null}
        </div>
    );
}

const Game = () => {
    const [user, setUser] = useState<user | null>(null);
    const [gameAddress, setGameAddress] = useState<string | null>(null);

    const disconnect = () => setUser(null);
    const connectInMemory = async (): Promise<{ signer: DekuSigner, address: string }> => {
        const secrets = [
            "edsk4Jex4ueokTLnc7xnkdXBZPfo1MQNeNPeDEmbjo7Wk2DSdVwjJD",
            "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
        ];
        const secret = secrets[Math.floor(Math.random() * secrets.length)];
        const tezosSigner = new InMemorySigner(secret);
        const signer = fromMemorySigner(tezosSigner);
        const address = await signer.publicKeyHash();
        console.log(address);
        return { signer, address }
    }

    const connectBeaconWallet = async () => {
        const dAppClient = new DAppClient({ name: "Tic Tac Toe" });
        await dAppClient.requestPermissions();
        const signer = fromBeaconSigner(dAppClient);
        const address = await signer.publicKeyHash();
        return { signer, address };
    }

    const connectWallet = async () => {
        connectInMemory()
            .then(setUser)
            .catch(console.error)
    }

    const resetGame = () => setGameAddress(null);

    if (user === null) return <Step1 connect={connectWallet} />;

    const deku = new DekuCClient({ dekuRpc, ligoRpc, signer: user.signer });

    if (gameAddress === null) return <Step2 deku={deku} user={user} disconnect={disconnect} setGameAddress={setGameAddress} />

    return <Step3 deku={deku} user={user} gameAddress={gameAddress} disconnect={disconnect} />
}

export default Game;