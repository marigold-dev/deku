export type Address = string;

export type Cell = "Cross" | "Circle" | "Empty"

export type CellId = number;

export type GameState = {
    type: "PlayerTurn" | "Winner",
    payload: Address
}

export interface State {
    game: Array<Cell>
    players: {
        player1: Address,
        player2: Address
    },
    gameState: GameState
}

// Parse the state retrieved from the deku-c-toolkit 
export const parseState = (state: any) => {
    const cell1 = state[0][0][0][0][0][0];
    const cell2 = state[0][0][0][0][0][1];
    const cell3 = state[0][0][0][0][1][0];
    const cell4 = state[0][0][0][0][1][1];
    const cell5 = state[0][0][0][1][0][0];
    const cell6 = state[0][0][0][1][0][1];

    const cell7 = state[0][0][0][1][1][0];
    const cell8 = state[0][0][0][1][1][1];
    const cell9 = state[0][0][1]

    const game = [
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

    const gameState: GameState = state[0][1].left
        ? { type: "PlayerTurn", payload: state[0][1].left }
        : { type: "Winner", payload: state[0][1].right }

    const player1 = state[1][0]
    const player2 = state[1][1];

    const players = {
        player1,
        player2
    }

    return {
        game,
        players,
        gameState
    };
}

export const play = (cellId: CellId) => {
    // return ["Int", cellId + ""];
    return ["Union", ["Right", ["Int", cellId + ""]]]
} 