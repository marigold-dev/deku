import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled, { css } from 'styled-components';

import { CompileAction } from '../../redux/actions/compile';
import { CompileFunctionAction } from '../../redux/actions/compile-function';
import { DeployAction } from '../../redux/actions/deploy';
import { DryRunAction } from '../../redux/actions/dry-run';
import { EvaluateFunctionAction } from '../../redux/actions/evaluate-function';
import { EvaluateValueAction } from '../../redux/actions/evaluate-expr';
import { GenerateDeployScriptAction } from '../../redux/actions/generate-deploy-script';
import { AppState } from '../../redux/app';
import { ChangeDispatchedAction, ChangeSelectedAction, CommandState } from '../../redux/command';
import { CommandType } from '../../redux/types';
import { Option, Select } from '../form/select';
import { CompilePaneComponent } from './compile-pane';
import DeployPaneComponent from './deploy-pane';
import { DryRunPaneComponent } from './dry-run-pane';
import CompileFunctionPaneComponent from './compile-function-pane';
import { EvaluateFunctionPaneComponent } from './evaluate-function-pane';
import { EvaluateValuePaneComponent } from './evaluate-expr-pane';
import { GenerateDeployScriptPane } from './generate-deploy-script-pane';

const Container = styled.div<{ visible?: boolean }>`
  position: absolute;
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  padding: 1em 1em 0 1em;

  display: flex;
  flex-direction: column;

  transform: translateX(-100%);
  transition: transform 0.2s ease-in;

  ${props =>
    props.visible ?
    css`
      transform: translateX(0px);
    ` : css`
      visibility: hidden;
    `} 
`;

const CommonActionsGroup = styled.div`
  display: flex;
  align-items: center;
`;

const RunButton = styled.div`
  cursor: pointer;
  user-select: none;

  display: flex;
  justify-content: center;
  align-items: center;
  flex: 1;
  min-height: 2em;
  min-width: 3em;
  margin-left: 1em;

  color: white;
  background-color: var(--orange);

  &:hover {
    box-shadow: var(--box-shadow);
  }
`;

const SelectCommand = styled(Select)`
  flex: 2;

  &:hover {
    box-shadow: var(--box-shadow);
  }
`;

function createAction(command: CommandType) {
  switch (command) {
    case CommandType.Compile:
      return new CompileAction();
    case CommandType.CompileFunction:
      return new CompileFunctionAction();
    case CommandType.DryRun:
      return new DryRunAction();
    case CommandType.Deploy:
      return new DeployAction();
    case CommandType.EvaluateValue:
      return new EvaluateValueAction();
    case CommandType.EvaluateFunction:
      return new EvaluateFunctionAction();
    case CommandType.GenerateDeployScript:
      return new GenerateDeployScriptAction();
    default:
      throw new Error('Unsupported command');
  }
}

export const ConfigureTabComponent = (props: {
  selected?: boolean;
  onRun?: () => void;
}) => {
  const dispatchedAction = useSelector<
    AppState,
    CommandState['dispatchedAction']
  >(state => state.command && state.command.dispatchedAction);

  const command = useSelector<AppState, CommandState['selected']>(
    state => state.command && state.command.selected
  );

  const dispatch = useDispatch();

  return (
    <Container visible={props.selected}>
      <CommonActionsGroup>
        <SelectCommand
          id="command-select"
          value={command}
          onChange={command => {
            dispatch({ ...new ChangeSelectedAction(command) });
          }}
        >
          <Option value={CommandType.Compile}>Compile Contract</Option>
          <Option value={CommandType.CompileFunction}>Compile Expression</Option>
          <Option value={CommandType.Deploy}>Deploy</Option>
          <Option value={CommandType.DryRun}>Dry Run</Option>
          <Option value={CommandType.EvaluateFunction}>Evaluate Function</Option>
          <Option value={CommandType.EvaluateValue}>Evaluate Value</Option>
          <Option value={CommandType.GenerateDeployScript}>Generate Deploy Script</Option>
        </SelectCommand>
        <RunButton
          id="run"
          onClick={() => {
            if (dispatchedAction) {
              dispatchedAction.cancel();
            }

            const newAction = createAction(command);
            dispatch(newAction.getAction());
            dispatch({ ...new ChangeDispatchedAction(newAction) });

            props.onRun!();
          }}
        >
          Run
        </RunButton>
      </CommonActionsGroup>
      {(command === CommandType.Compile && (
        <CompilePaneComponent></CompilePaneComponent>
        )) ||
        (command === CommandType.CompileFunction && (
          <CompileFunctionPaneComponent></CompileFunctionPaneComponent>
        )) ||
        (command === CommandType.DryRun && (
          <DryRunPaneComponent></DryRunPaneComponent>
        )) ||
        (command === CommandType.Deploy && (
          <DeployPaneComponent></DeployPaneComponent>
        )) ||
        (command === CommandType.EvaluateFunction && (
          <EvaluateFunctionPaneComponent></EvaluateFunctionPaneComponent>
        )) ||
        (command === CommandType.EvaluateValue && (
          <EvaluateValuePaneComponent></EvaluateValuePaneComponent>
        )) ||
        (command === CommandType.GenerateDeployScript && (
          <GenerateDeployScriptPane></GenerateDeployScriptPane>
        ))}
    </Container>
  );
};
