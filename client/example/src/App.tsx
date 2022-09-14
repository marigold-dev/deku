import React from "react";
import logo from "./logo.png"
import CSS from 'csstype';

const containerStyle: CSS.Properties = {
    textAlign: "center"
}

const App = () => {
    return (
        <div style={containerStyle}>
            <img className="App-logo" src={logo}/>
        </div>
    );
}

export default App