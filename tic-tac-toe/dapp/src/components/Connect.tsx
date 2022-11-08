interface ConnectWalletProperty {
    connect: () => void,
    address: string | undefined | null,
    disconnect: () => void | undefined | null
}

interface ConnectProperty {
    connect: () => void,
}

interface Connected {
    address: string,
    disconnect: () => void
}

const Connect = ({connect}: ConnectProperty) => {
    return <div id="connect-wallet" onClick={connect}>Connect</div>
}

const Connected = ({address, disconnect}: Connected) => {
    const formatedAddress = 
        address 
            ? `${address.slice(0, 5)}...${address.slice(address.length - 5, address.length)}`
            : undefined;
    return <div id="connect-wallet" className="tooltip" onClick={disconnect}>
        <div className="tooltiptext">Disconnect</div>
        {`Connected as ${formatedAddress}`}
    </div>
}

const ConnectWallet = ({connect, disconnect, address}: ConnectWalletProperty) => {
    const isConnected = disconnect !== undefined && disconnect !== null && address !== undefined && address !== null;
    if(!isConnected) return <Connect connect={connect}/>
    else return <Connected disconnect={disconnect} address={address}/>
}

export default ConnectWallet;