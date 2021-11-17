import React from 'react';
import styles from './styles.module.css';

function SyntaxSwitch(props) {
  return React.createElement("select", {
    className: styles.syntaxSwitch,
    defaultValue: props.syntax,
    onChange: e => props.onSyntaxChange(e.target.value)
  }, React.createElement("option", {
    value: "pascaligo"
  }, "PascaLIGO"), React.createElement("option", {
    value: "cameligo"
  }, "CameLIGO"), React.createElement("option", {
    value: "reasonligo"
  }, "ReasonLIGO"), React.createElement("option", {
    value: "jsligo"
  }, "JsLIGO"));
}

export default SyntaxSwitch;