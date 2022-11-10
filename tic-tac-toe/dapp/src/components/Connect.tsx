interface ConnectWalletProperty {
    connect: () => void,
}


const Connect = ({ connect }: ConnectWalletProperty) => {
    return <div id="connect-wallet" onClick={connect}>Connect Wallet</div>
}

export default Connect;