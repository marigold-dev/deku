import React, { useEffect } from "react";
import Board from '../components/Board';
import { useDispatch, useSelector } from "react-redux";
import Layout from "../components/Layout";
import { State as GameState } from "../state/reducer";
import CreateGame from "../components/CreateGame";
import { Contract } from "@marigold-dev/deku-c-toolkit";
import { parseState } from "../core";

const Game = () => {
    const dispatch = useDispatch();
    const address = useSelector<GameState, string | undefined>(state => state.user.type === "CONNECTED" ? state.user.address : undefined);
    const step = useSelector<GameState, string>(state => state.game.type);
    const game = useSelector<GameState, any>(state => state.game.type === "PLAYING" ? state.game.game : undefined);
    const gameAddress = useSelector<GameState, string>(state => state.game.type === "PLAYING" ? state.game.address : "");
    const contract = useSelector<GameState, Contract | undefined>(state => {
        if (state.game.type === "STARTED") return state.game.contract;
        if (state.game.type === "PLAYING") return state.game.contract;
        return undefined
    })

    // Used to join an existing party
    useEffect(() => {
        const queryString = window.location.search;
        const urlParams = new URLSearchParams(queryString);
        const gameAddress = urlParams.get("game");
        dispatch({ type: "TRY_JOINING_FROM_URL", payload: { contract: gameAddress } });
    }, [dispatch]);

    // Used to subscribe to the state of the contract
    useEffect(() => {
        if (contract && address) {
            contract.onNewState(state => {
                const parsed = parseState(state, address);
                dispatch({ type: "FETCH_GAME_SUCCESS", payload: parsed })
            });
            return () => {
                // TODO: add a function in the toolkit to clean the callback
                contract.onNewState(() => { })
            };
        }
    }, [dispatch, contract, address]);

    const disconnect = () => dispatch({ type: "DISCONNECT" });
    const connect = () => dispatch({ type: "CONNECT" });
    const createGame = () => dispatch({ type: "CREATE_GAME" });
    const play = ({ id }: { id: number }) => dispatch({ type: "PLAY", payload: id });

    return <Layout user={address} connect={connect} disconnect={disconnect}>
        {step === "INIT"
            ? <CreateGame createGame={createGame} />
            : step === "CREATING_GAME"
                ? <div>Joining the game</div>
                : step === "PLAYING"
                    ? <Board game={game} play={play} address={gameAddress} />
                    : <React.Fragment />
        }
    </Layout>
}

export default Game;