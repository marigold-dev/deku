import './App.css';
import { DekuCClient } from '@marigold-dev/deku-c-toolkit';
import { fromMemorySigner } from '@marigold-dev/deku-toolkit';
import { InMemorySigner } from '@taquito/signer';
import { useEffect, useState } from 'react';
import Board from './components/Board';
import { State, parseState } from "./core";

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

const App = () => {
  const [state, setState] = useState<State | null>(null);

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
    contract.getState()
      .then(parseState)
      .then(setState)
      .catch(console.error);
  }, [])

  return (
    <div className="App">
      <button onClick={originate}>Originate</button>
      {state && <Board cells={state.game} onCellClick={onClick} />}
    </div>
  );
}

export default App;