interface ConnectWalletProperty {
    connect: () => void,
}


const Connect = ({ connect }: ConnectWalletProperty) => {
    return <button id="connect" onClick={connect}>Connect Wallet</button>
}

export default Connect;