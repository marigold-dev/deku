interface ConnectedProperty {
    address: string,
    disconnect: () => void
}

const Connected = ({ address, disconnect }: ConnectedProperty) => {
    const formatedAddress =
        address
            ? `${address.slice(0, 5)}...${address.slice(address.length - 5, address.length)}`
            : undefined;
    return <button id="connect" className="tooltip clickable" onClick={disconnect}>
        <div className="tooltiptext">Disconnect</div>
        {`Connected as ${formatedAddress}`}
    </button>
}
export default Connected