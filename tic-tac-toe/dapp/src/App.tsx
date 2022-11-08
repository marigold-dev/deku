import './App.css';
import { DekuCClient } from '@marigold-dev/deku-c-toolkit';
import { fromMemorySigner } from '@marigold-dev/deku-toolkit';
import { InMemorySigner } from '@taquito/signer';
import { useEffect, useState } from 'react';

const dekuRpc = "http://0.0.0.0:8080";
const ligoRpc = "http://0.0.0.0:9090";
const secret = "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq";
const tezosSigner = new InMemorySigner(secret);
const signer = fromMemorySigner(tezosSigner);

const deku = new DekuCClient({ dekuRpc, ligoRpc, signer });

const jsligo = `
type cell = ["Cross"] | ["Circle"] | ["Empty"]

type gameState = 
  | ["PlayerTurn", address]
  | ["Winner", address]

type storage = {
  game: list<cell>,
  players: {
    player1: address,
    player2: address
  },
  gameState: gameState
};

const empty: storage = {
  game: list([
    Cross(), Empty(), Circle(), 
    Empty(), Empty(), Empty(), 
    Empty(), Empty(), Empty()
  ]),
  players: {
    player1: "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address, // The two players are the same
    player2: "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address,
  },
  gameState: PlayerTurn("tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address)
};

type return_ =

  [list<operation>,
   storage];

const main = 
  (_action: unit, storage: storage): return_ => {return [list([]),
    storage]};
`;

const initialStorage: any = '(Pair (Pair { Left (Right Unit) ; Right Unit ; Left (Left Unit) ; Right Unit ; Right Unit ; Right Unit ; Right Unit ; Right Unit ; Right Unit } (Left "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")) "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")'

const address = "DK1MYaA26NdjDT9eNWnKvd2LXye1vhPFyxDm"


const Cell = ({ id, state, onClick }: { id: number, state: string, onClick: ({ id, state }: { id: number, state: string }) => void }) => {
  const character = state === "cross"
    ? "X"
    : state === "circle"
      ? "O"
      : ""
  return (
    <div className="cell" onClick={() => onClick({ id, state })}>{character}</div>);
}

type CellType = "Cross" | "Circle" | "Empty"
interface GameState {
  type: "PlayerTurn" | "Winner",
  payload: string
}

interface State {
  game: Array<CellType>,
  players: {
    player1: string,
    player2: string,
  },
  gameState: {
    type: "PlayerTurn" | "Winner",
    payload: string
  }
}

const stateParser = (state: any): State => {
  const game: Array<CellType> = state[0][0].map((cell: any) => {
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



const App = () => {
  const [state, setState] = useState(null);
  const game: Array<string> = state || []; // Should be a list

  const originate = () => {
    deku.originateContract({ kind: "jsligo", code: jsligo, initialStorage })
      .then(console.log)
      .catch(console.error)
  }

  const onClick = ({ id, state }: { id: number, state: string }) => {
    console.log(id);
    console.log(state);
  }

  useEffect(() => {
    const contract = deku.contract(address);
    console.log("hello");
    contract.getState()
      .then(stateParser)
      .then(console.log)
      .catch(console.error);
  }, [])

  return (
    <div className="App">
      <button onClick={originate}>Originate</button>
      <div id="game">
        {game.map((state, i) => <Cell id={i} state={state} onClick={onClick} />)}
      </div>
    </div>
  );
}

export default App;