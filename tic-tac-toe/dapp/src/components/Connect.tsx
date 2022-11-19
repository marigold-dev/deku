interface ConnectWalletProperty {
    connect: () => void,
}


const Connect = ({ connect }: ConnectWalletProperty) => {
    return <button id="connect" className="clickable" onClick={connect}>Connect Wallet</button>
}

export default Connect;