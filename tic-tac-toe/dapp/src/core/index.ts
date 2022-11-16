export type Address = string;

export type Cell = "Cross" | "Circle" | "Empty"

export type CellId = number;

export type step =
    | { type: "WAITING_OPPONENT_TO_JOIN" }
    | { type: "YOUR_TURN" }
    | { type: "OPPONENT_TURN" }
    | { type: "WON" }
    | { type: "LOST" }
    | { type: "DRAW" }

export interface State {
    board: Array<Cell>
    opponent: Address,
    step: step,
}

// Parse the state retrieved from the deku-c-toolkit 
export const parseState = (state: any, currentPlayer: string) => {
    const cell1 = state[0][0][0][0][0][0];
    const cell2 = state[0][0][0][0][0][1];
    const cell3 = state[0][0][0][0][1][0];
    const cell4 = state[0][0][0][0][1][1];
    const cell5 = state[0][0][0][1][0][0];
    const cell6 = state[0][0][0][1][0][1];

    const cell7 = state[0][0][0][1][1][0];
    const cell8 = state[0][0][0][1][1][1];
    const cell9 = state[0][0][1]

    const board = [
        cell1,
        cell2,
        cell3,
        cell4,
        cell5,
        cell6,
        cell7,
        cell8,
        cell9,
    ].map(cell => {
        if (cell && cell.left && cell.left.right === null) return "Cross";
        else if (cell && cell.right === null) return "Empty";
        else return "Circle"
    })

    const step = state[0][1].left
        ? state[0][1].left === currentPlayer
            ? { type: "YOUR_TURN" }
            : { type: "OPPONENT_TURN" }
        : state[0][1].right === currentPlayer
            ? { type: "WON" }
            : { type: "LOST" }

    const player1 = state[1][0]
    const player2IsNone = state[1][1].none;
    const opponent = player1 === currentPlayer && state[1][1].some ? state[1][1].some : player1;
    return {
        board,
        opponent,
        step: player2IsNone ? { type: "WAITING_OPPONENT_TO_JOIN" } : step,
    };
}

export const makePlayPayload = (cellId: CellId) => {
    // return ["Int", cellId + ""];
    return ["Union", ["Right", ["Int", cellId + ""]]]
}

export const makeInitStorage = (player1: string, player2: string) => {
    return `(Pair(Pair(Pair(Pair(Pair(Pair(Left(Right Unit))(Right Unit))(Left(Left Unit))(Right Unit))(Pair(Right Unit)(Right Unit))(Right Unit)(Right Unit))(Right Unit))(Left "${player1}")) "${player1}" "${player2}")`;
}

export const makeJoinPayload = () => {
    return ["Union", ["Left", ["Unit"]]]
}