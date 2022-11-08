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
type storage = {game: list<string>};

const empty: storage = {
  game: list([
    "cross",
    "empty",
    "circle",

    "empty",
    "empty",
    "empty",

    "empty",
    "empty",
    "empty"
])};

type return_ =
  [list<operation>,
   storage];

const main = 
  (_action: unit, storage: storage): return_ => {return [list([]),
    storage]};
`;

const initialStorage: any = '{ "cross" ; "empty" ; "circle" ; "empty" ; "empty" ; "empty" ; "empty" ; "empty" ; "empty" }'

const address = "DK1DhzAPHiR6oQc1oYthYAdUmMLB9Z7z1C5x"

const Cell = ({ id, state, onClick }: { id: number, state: string, onClick: ({ id, state }: { id: number, state: string }) => void }) => {
  const character = state === "cross"
    ? "X"
    : state === "circle"
      ? "O"
      : ""
  return (
    <div className="cell" onClick={() => onClick({ id, state })}>{character}</div>);
}

const App = () => {
  const [state, setState] = useState(null);
  console.log(state);
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
    contract.getState()
      .then(setState)
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