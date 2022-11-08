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
    const game: Array<Cell> = state[0][0].map((cell: any) => {
        if (cell && cell.left && cell.left.right === null) return "Cross";
        else if (cell && cell.right === null) return "Empty";
        else return "Circle"
    });
    const players = {
        player1: state[1][0],
        player2: state[1][1],
    }
    const gameState: GameState = state[0][1].left
        ? { type: "PlayerTurn", payload: state[0][1].left }
        : { type: "Winner", payload: state[0][1].right }
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