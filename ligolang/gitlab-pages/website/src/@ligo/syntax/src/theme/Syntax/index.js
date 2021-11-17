import React from 'react';
import SyntaxContext from './SyntaxContext'

function Syntax(props) {    
  return (
    <SyntaxContext.Consumer>
      {(syntax => {
         if (syntax === props.syntax) {
             return props.children
         } else {
             return <></>
         }
      })}
    </SyntaxContext.Consumer>
  )
}

export default Syntax

export { SyntaxContext }