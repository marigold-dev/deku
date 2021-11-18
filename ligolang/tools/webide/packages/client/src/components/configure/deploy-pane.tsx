import React, {FC} from 'react';
import { useDispatch, connect } from 'react-redux';
import styled from 'styled-components';

import { ChangeEntrypointAction, ChangeStorageAction, UseNetworkAction, UseSignerAction, networkType, signerType } from '../../redux/deploy';
import { CheckboxComponent } from '../form/checkbox';
import { AccessFunctionLabel, Group, HGroup, Input, Label, Textarea } from '../form/inputs';
import { Option, Select } from '../form/select';

const Container = styled.div``;

const Checkbox = styled(CheckboxComponent)`
  margin-right: 0.3em;
`;

const Hint = styled.span`
  font-style: italic;
  font-size: 0.8em;
`;

const SelectCommand = styled(Select)`
  flex: 2;

  &:hover {
    box-shadow: var(--box-shadow);
  }
`;

interface stateTypes {
  entrypoint?: string;
  storage?: string;
  useNetwork?: string
}

const DeployPaneComponent:FC<stateTypes> = (props) => {

  const {entrypoint, storage} = props
  let {useNetwork} = props

  const dispatch = useDispatch();

  const setSigner = (isSelected) => {
    if(isSelected && useNetwork !== networkType.Mainnet){
      dispatch({ ...new UseSignerAction(signerType.Sign) })
    } else {
      dispatch({ ...new UseSignerAction(signerType.Beacon) })
    }
  }

  return (
    <Container>
      <Group>
      <Label htmlFor="storage">Choose a Network</Label>
      <SelectCommand
          id="command-select"
          value={useNetwork}
          onChange={network => {
            useNetwork = network
              dispatch({ ...new UseNetworkAction(network) })
              if (useNetwork !== networkType.Mainnet) {
                setSigner(true)
              } else {
                setSigner(false)
              }
          }}
        >
          <Option value={networkType.Granadanet}>Granadanet</Option>
          <Option value={networkType.Florencenet}>Florencenet</Option>
          <Option value={networkType.Mainnet}>Mainnet</Option>
        </SelectCommand>
        <AccessFunctionLabel htmlFor="entrypoint"></AccessFunctionLabel>
        <Input
          id="entrypoint"
          value={entrypoint}
          onChange={ev =>
            dispatch({ ...new ChangeEntrypointAction(ev.target.value) })
          }
        ></Input>
      </Group>
      <Group>
        <Label htmlFor="storage">Storage</Label>
        <Textarea
          id="storage"
          rows={9}
          value={storage}
          onChange={ev =>
            dispatch({ ...new ChangeStorageAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
      {useNetwork && ( useNetwork === networkType.Florencenet || useNetwork === networkType.Granadanet) &&
      <HGroup>
        <Checkbox
          checked={true}
          onChanged={(value) => setSigner(value)}
        ></Checkbox>
        <Label htmlFor="tezbridge">
          We'll sign for you
          <br />
          <Hint>Got your own key? Deselect to sign with Beacon</Hint>
        </Label>
      </HGroup>
      }
    </Container>
  );
};

function mapStateToProps(state) {
  const { deploy } = state
  return { 
    entrypoint: deploy.entrypoint,
    storage: deploy.storage,
    useNetwork: deploy.network,
    useSigner: deploy.signer
   }
}

export default connect(mapStateToProps, null)(DeployPaneComponent)
