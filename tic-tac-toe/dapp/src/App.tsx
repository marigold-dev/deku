import './App.css';
import { DekuCClient } from '@marigold-dev/deku-c-toolkit';
import { fromMemorySigner } from '@marigold-dev/deku-toolkit';
import { InMemorySigner } from '@taquito/signer';
import { useEffect, useState } from 'react';
import Board from './components/Board';
import { State, parseState, play } from "./core";

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

type parameter =
  | ["Play", nat]

type return_ = [list<operation>, storage];


const aux: <T, U> (fct: (i: nat, elt:T) => U, index:nat, ls:list<T>, acc:list<U>) => list<U> = (f, index, ls, acc) => {
  return match(ls, list([
    ([]: list<T>) => acc,
    ([head, ...tl]: list<T>) => {
      let next = f(index, head);
      let nextAcc = list([next, ...acc]);
      return aux(f, index + (1 as nat), tl, nextAcc)
    }
  ]))
};

const mapi: <T, U> (fct:(i: nat, elt: T) => U , ls: list<T>) => list<U> = (f,ls) => {
  let acc: list<U> = list([]);
  return aux(f, 0 as nat, ls, acc)
}


const updateCell = (cellId: nat, value:cell, game: list<cell>) => {
  let apply = (index:nat, elt: cell) => {
    if(cellId != index) return elt
    return match(elt, {
      Cross: () => failwith("Already set"),
      Circle: () => failwith("Already set"),
      Empty: () => value
    })
  };
  return mapi(apply, game)
}

const getSymbol = (player: address, storage: storage): cell => {
  if(player == storage.players.player1) return Cross()
  else return Circle()
}

const play = (player: address, cellId: nat, storage:storage): storage => {
  // let current_player = match(storage.gameState, {
  //   PlayerTurn: player => player,
  //   Winner: _ => failwith("Game is ended")
  // });
  // if(current_player != player) {return failwith("Not your turn")};
  if(cellId > (8 as nat)) {return failwith("Wrong cellId")};
  const value = getSymbol(player, storage);
  const _ = updateCell(cellId, value, storage.game);
  return storage
}

const main = (parameter: parameter, storage: storage): return_ => {
    let player = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address;
    let operations: list<operation> = list([]);
    let storage = (match (parameter, {
      Play: cellId => play (player, cellId, storage)
    }));
    return [operations, storage]
}
`;

const initialStorage: any = '(Pair (Pair { Left (Right Unit) ; Right Unit ; Left (Left Unit) ; Right Unit ; Right Unit ; Right Unit ; Right Unit ; Right Unit ; Right Unit } (Left "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")) "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")'

const App = () => {
  const [state, setState] = useState<State | null>(null);
  const [address, setAddress] = useState<string>("");

  const originate = () => {
    deku.originateContract({ kind: "jsligo", code: jsligo, initialStorage })
      .then(({ address }) => new Promise<string>(resolve => setTimeout(() => resolve(address), 5000)))
      .then(setAddress)
      .then(() => console.log("originated"))
      .catch(console.error)
  }

  const onClick = ({ id, state }: { id: number, state: string }) => {
    console.log("click");
    const move = play(id);
    console.log(move)
    deku.contract(address)
      .invoke(play(id))
      .then(console.log);
  }

  useEffect(() => {
    const contract = deku.contract(address);
    contract.getState()
      .then(parseState)
      .then(setState)
      .catch(console.error);
  }, [address])

  return (
    <div className="App">
      <button onClick={originate}>Originate</button>
      {state && <Board cells={state.game} onCellClick={onClick} />}
    </div>
  );
}

export default App;