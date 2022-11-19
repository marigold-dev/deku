type CreateGameProperty = {
    createGame: () => void
}

const CreateGame = ({ createGame }: CreateGameProperty) => {
    return (
        <div className="button clickable" onClick={createGame}>Create a game</div>
    );
}

export default CreateGame;