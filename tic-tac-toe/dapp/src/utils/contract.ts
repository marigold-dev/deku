const contract = `
type cell = ["Cross"] | ["Circle"] | ["Empty"]

type gameState = 
  | ["PlayerTurn", address]
  | ["Winner", address]

type game = [cell, cell, cell, cell, cell, cell, cell, cell, cell];

type storage = {
  game: game,
  players: {
    player1: address,
    player2: address
  },
  gameState: gameState
};

const mapi : (fct:(i: nat, elt: cell) => cell , ls: game) => game = (f,ls) => {
  return [
    f(0 as nat, ls[0]),
    f(1 as nat, ls[1]),
    f(2 as nat, ls[2]),
    f(3 as nat, ls[3]),
    f(4 as nat, ls[4]),
    f(5 as nat, ls[5]),
    f(6 as nat, ls[6]),
    f(7 as nat, ls[7]),
    f(8 as nat, ls[8])
  ]
}

const empty: storage = {
  game: [
    Cross(), Empty(), Circle(), 
    Empty(), Empty(), Empty(), 
    Empty(), Empty(), Empty()
  ],
  players: {
    player1: "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address, // The two players are the same
    player2: "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address,
  },
  gameState: PlayerTurn("tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address)
};

type parameter =
  | ["Play", nat]
  | ["Join"]

type return_ = [list<operation>, storage];

const updateCell = (cellId: nat, value:cell, game: game) => {
  let apply = (index:nat, elt: cell) => {
    if(cellId != index) return elt
    return match(elt, {
      Cross: () => failwith("Already set: cross"),
      Circle: () => failwith("Already set: circle"),
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
  const {game, players, gameState} = storage;
  let current_player = match(storage.gameState, {
    PlayerTurn: player => player,
    Winner: _ => failwith("Game is ended")
  });
  if(current_player != player) {return failwith("Not your turn")};
  if(cellId > (8 as nat)) {return failwith("Wrong cellId")};
  const value = getSymbol(player, storage);
  const game = updateCell(cellId, value, game);
  return {game, players, gameState}
}

const main = (parameter: parameter, storage: storage): return_ => {
    let player = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address;
    let operations: list<operation> = list([]);
    let storage = (match (parameter, {
      Play: cellId => play (player, cellId, storage),
      Join: () => failwith("Join is not yet implemented")
    }));
    return [operations, storage]
}

`;

export default contract;