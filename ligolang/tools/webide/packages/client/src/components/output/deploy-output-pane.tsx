import React from 'react';
import { connect } from 'react-redux';
import styled from 'styled-components';

const Container = styled.div<{ visible?: boolean }>`
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: auto;
`;

const Output = styled.div`
  flex: 1;
  padding: 0.5em 0.5em 0.5em 0.5em;
  display: flex;
  flex-direction: column;
`;

const Pre = styled.pre`
  padding: 0.5em;
  margin: 0 -0.5em;
  overflow: hidden;
  height: 100%;
  width: -webkit-fill-available;
  white-space: normal;
`;

const DeployOutputPane = (props) => {
const {contract, output, network} = props
let networkUrlPart = network
  return (
    <Container>
      <Output id="output">
        {contract && (
          <div>
            The contract was successfully deployed to the {network} test network.
            <br />
            <br />
            View your new contract using{' '}
            <a
              target="_blank"
              rel="noopener noreferrer"
              href={`https://better-call.dev/${networkUrlPart}/${contract}`}
            >
              Better Call Dev
            </a>
            !
            <br />
            <br />
            <b>The address of your new contract is: </b>
            <i>{contract}</i>
            <br />
            <br />
            <b>The initial storage of your contract is: </b>
          </div>
        )}
        {output && <Pre>{output}</Pre>}
      </Output>
    </Container>
  );
};

function mapStateToProps(state) {
  const { result, deploy } = state
  return { 
    output: result.output,
    contract: result.contract,
    network: deploy.network
   }
}

export default connect(mapStateToProps, null)(DeployOutputPane)
