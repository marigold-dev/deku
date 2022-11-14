import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit"
import { loop, Cmd } from "redux-loop"
import { connectCmd, createGameCmd, fetchGameCmd, joinCmd, playCmd } from "./cmd"
import { State as ContractState } from "../core";

/// State
type userState =
    { type: "DISCONNECTED" } | { type: "CONNECTED", deku: DekuCClient, address: string };

type gameState =
    | { type: "INIT" }
    | { type: "CREATING_GAME" }
    | { type: "STARTED", contract: Contract, address: string }
    | { type: "PLAYING", contract: Contract, game: ContractState, address: string }


export type State = {
    user: userState,
    game: gameState,
}

export const initialState: State = {
    user: {
        type: "DISCONNECTED",
    },
    game: {
        type: "INIT",
    }
}


/// Actions
type ConnectAction = {
    type: "CONNECT",
    next: Action
}

type ConnectedAction = {
    type: "CONNECTED",
    payload: {
        deku: DekuCClient,
        address: string,
    }
    next: Action,
}

type DisconectAction = {
    type: "DISCONNECT",
}

type CreateGame = {
    type: "CREATE_GAME"
}

type CreateGameSuccess = {
    type: "CREATE_GAME_SUCCESS",
    payload: {
        address: string
    }
}

type CreateGameFailure = {
    type: "CREATE_GAME_FAILURE",
}

type FetchGame = {
    type: "FETCH_GAME",
}

type FetchGameSuccess = {
    type: "FETCH_GAME_SUCCESS",
    payload: ContractState
}

type FetchGameFailure = {
    type: "FETCH_GAME_FAILURE"
}

type Play = {
    type: "PLAY",
    payload: number,
}

type TryJoiningFromUrl = {
    type: "TRY_JOINING_FROM_URL",
    payload: {
        contract: string | undefined
    }
}

type Join = {
    type: "JOIN",
    payload: {
        contract: string
    }
}

type JoinSuccess = {
    type: "JOIN_SUCCESS",
    payload: {
        address: string
    }
}

type JoinFailure = {
    type: "JOIN_FAILURE"
}

type NotYourTurn = {
    type: "NOT_YOUR_TURN"
}

export type Action = ConnectAction
    | ConnectedAction
    | DisconectAction
    | CreateGame
    | CreateGameSuccess
    | CreateGameFailure
    | FetchGame
    | FetchGameSuccess
    | FetchGameFailure
    | Play
    | TryJoiningFromUrl
    | Join
    | JoinSuccess
    | JoinFailure
    | NotYourTurn

const reducer = (
    state: State = initialState,
    action: Action
) => {
    switch (action.type) {
        case "CONNECT": {
            const { next } = action;
            return loop(state, connectCmd(next));
        }
        case "CONNECTED": {
            const { next } = action;
            const { deku, address } = action.payload;
            const nextAction = next ? Cmd.action(next) : Cmd.none;
            return loop({
                ...state,
                user: {
                    type: "CONNECTED",
                    deku,
                    address
                }
            }, nextAction);
        }
        case "DISCONNECT": {
            return initialState
        }
        case "CREATE_GAME": {
            if (state.user.type !== "CONNECTED") return state;
            const { deku, address: player } = state.user;
            const nextState = {
                ...state,
                game: {
                    type: "CREATING_GAME"
                }
            }
            return loop(nextState, createGameCmd(deku, player))
        }
        case "CREATE_GAME_SUCCESS": {
            if (state.user.type !== "CONNECTED") return state;
            const { deku } = state.user;
            const { address } = action.payload;
            const contract = deku.contract(address);
            const nextState = {
                ...state, game: {
                    type: "STARTED",
                    contract,
                    address
                }
            }
            return loop(nextState, Cmd.action({ type: "FETCH_GAME", payload: { address } }))
        }
        case "FETCH_GAME": {
            if (state.user.type !== "CONNECTED") return state;
            if (state.game.type === "INIT") return state;
            if (state.game.type === "CREATING_GAME") return state;
            const { address } = state.user;
            const { contract } = state.game;
            return loop(state, fetchGameCmd(contract, address));
        }
        case "FETCH_GAME_SUCCESS": {
            if (state.user.type !== "CONNECTED") return state;
            if (state.game.type === "INIT") return state;
            if (state.game.type === "CREATING_GAME") return state;
            const game = action.payload;
            const { contract } = state.game;
            const nextState = {
                ...state, game: {
                    type: "PLAYING",
                    contract: contract,
                    game,
                    address: state.game.address
                }
            }
            return nextState;
        }
        case "FETCH_GAME_FAILURE": {
            // How to handle this error ? reset the game ? 
            return state;
        }
        case "PLAY": {
            if (state.user.type !== "CONNECTED") return state;
            if (state.game.type !== "PLAYING") return state;
            const { contract } = state.game;
            const id = action.payload;
            if (state.game.game.step.type !== "YOUR_TURN") {
                return loop(state, Cmd.action({ type: "NOT_YOUR_TURN" }));
            } // TODO should we add another action ? Like CHECK_PLAY => PLAY => PLAY_SUCCESS
            return loop(state, playCmd(contract, id));
        }
        case "TRY_JOINING_FROM_URL": {
            const { contract } = action.payload;
            if (!contract) return state;
            return loop(state, Cmd.action({ type: "JOIN", payload: { contract } }));
        }
        case "JOIN": {
            if (state.user.type === "DISCONNECTED") {
                const connect = {
                    type: "CONNECT",
                    next: action
                };
                return loop(state, Cmd.action(connect));
            };
            const { deku } = state.user;
            const { contract: address } = action.payload;
            const contract = deku.contract(address);
            return loop(state, joinCmd(contract, address));
        }
        case "JOIN_SUCCESS": {
            const { address } = action.payload;
            const nextAction = { type: "CREATE_GAME_SUCCESS", payload: { address } };
            return loop(state, Cmd.action(nextAction));
        }
        case "JOIN_FAILURE": {
            return state;
        }
        case "NOT_YOUR_TURN":
        case "CREATE_GAME_FAILURE":
        default:
            return state;
    }
}

export default reducer;