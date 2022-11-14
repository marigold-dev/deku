type CreateGameProperty = {
    createGame: () => void
}

const CreateGame = ({ createGame }: CreateGameProperty) => {
    return (
        <div onClick={createGame}>Create a game</div>
    );
}

export default CreateGame;