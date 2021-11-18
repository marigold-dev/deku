import React from 'react';

import styles from './styles.module.css';

function SyntaxSwitch (props) {
    return (
        <select className={styles.syntaxSwitch}  defaultValue={props.syntax} onChange={e => props.onSyntaxChange(e.target.value)}>
            <option value="pascaligo">PascaLIGO</option>
            <option value="cameligo">CameLIGO</option>
            <option value="reasonligo">ReasonLIGO</option>
            <option value="jsligo">JsLIGO</option>
        </select>
    )
}

export default SyntaxSwitch