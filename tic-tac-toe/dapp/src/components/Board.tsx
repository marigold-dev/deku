import React from "react";
import { State, Cell as CellType, CellId } from "../core";
import Cell from "./Cell"
import Url from "./Url"

interface BoardProperty {
    game: State,
    play: ({ id, state }: { id: CellId, state: CellType }) => void,
    address: string,
}

const Board = ({ game, play, address }: BoardProperty) => {
    const { opponent, step } = game;

    const info =
        step.type === "WAITING_OPPONENT_TO_JOIN" ? "Waiting for your opponent to join"
            : step.type === "YOUR_TURN"
                ? "It's your turn"
                : step.type === "OPPONENT_TURN"
                    ? "Waiting for your opponent"
                    : step.type === "WON"
                        ? "You won !!"
                        : step.type === "LOST"
                            ? "You lost :("
                            : "It's a draw"

    return (
        <div id="game">
            <div id="info">{info}</div>
            <div id="board">
                {game.board.map((state, i) => <Cell id={i} key={i} state={state} onClick={play} step={step} />)}
            </div>
            {step.type === "WAITING_OPPONENT_TO_JOIN" ? <React.Fragment /> : <div id="opponent">Your opponent is <b>{opponent}</b></div>}
            <Url address={address} />
        </div >
    );
}

export default Board