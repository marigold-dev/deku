import React from 'react';
import SyntaxContext from './SyntaxContext';

function Syntax(props) {
  return React.createElement(SyntaxContext.Consumer, null, syntax => {
    if (syntax === props.syntax) {
      return props.children;
    } else {
      return React.createElement(React.Fragment, null);
    }
  });
}

export default Syntax;
export { SyntaxContext };