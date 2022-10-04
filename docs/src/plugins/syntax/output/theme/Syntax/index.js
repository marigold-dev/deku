import React from 'react';
import SyntaxContext from './SyntaxContext';

function Syntax(props) {
  return /*#__PURE__*/React.createElement(SyntaxContext.Consumer, null, ({
    syntax
  }) => {
    if (syntax === props.syntax) {
      return props.children;
    } else {
      return /*#__PURE__*/React.createElement(React.Fragment, null);
    }
  });
}

export default Syntax;
export { SyntaxContext };