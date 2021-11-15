import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeEntrypointAction, ChangeMichelsonFormatAction, CompileState, MichelsonFormat } from '../../redux/compile';
import { CheckboxComponent } from '../form/checkbox';
import { AccessFunctionLabel, Group, HGroup, Input, Label } from '../form/inputs';

const Container = styled.div``;

const Checkbox = styled(CheckboxComponent)`
  margin-right: 0.3em;
`;

export const CompilePaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, CompileState['entrypoint']>(
    state => state.compile && state.compile.entrypoint
  );
  const michelsonFormat = useSelector<
    AppState,
    CompileState['michelsonFormat']
  >(state => state.compile && state.compile.michelsonFormat);

  return (
    <Container>
      <Group>
        <AccessFunctionLabel htmlFor="entrypoint"></AccessFunctionLabel>
        <Input
          id="entrypoint"
          value={entrypoint}
          onChange={ev =>
            dispatch({ ...new ChangeEntrypointAction(ev.target.value) })
          }
        ></Input>
      </Group>
      <HGroup>
        <Checkbox
          checked={michelsonFormat === MichelsonFormat.Json}
          onChanged={value =>
            dispatch({
              ...new ChangeMichelsonFormatAction(
                value ? MichelsonFormat.Json : MichelsonFormat.Text
              )
            })
          }
        ></Checkbox>
        <Label htmlFor="michelsonFormat">Output michelson in JSON format</Label>
      </HGroup>
    </Container>
  );
};
