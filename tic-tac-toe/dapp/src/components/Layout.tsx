import { ReactElement } from "react"
import Connect from "./Connect";
import Connected from "./Connected";

type LayoutProperty = {
    user: string | undefined,
    children: ReactElement,
    connect: () => void,
    disconnect: () => void,
}

const Layout = ({ user, children, connect, disconnect }: LayoutProperty) => {
    return (
        <div id="app">
            <div id="menu">
                {user
                    ? <Connected address={user} disconnect={disconnect} />
                    : <Connect connect={connect} />
                }
            </div>
            <div id="inner">
                {user
                    ? children
                    : <div id="not-connected">You have to be connected</div>
                }
            </div>
        </div>
    );
}

export default Layout