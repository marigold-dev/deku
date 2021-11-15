import {
  ActionType as ExamplesActionType,
  ChangeSelectedAction as ChangeSelectedExampleAction,
} from './examples';
import { Tool } from './types';

export enum ActionType {
  ChangeTool = 'generate-deploy-script-change-tool',
  ChangeEntrypoint = 'generate-deploy-script-change-entrypoint',
  ChangeStorage = 'generate-deploy-script-change-storage',
}

export interface GenerateDeployScriptState {
  tool: Tool;
  entrypoint: string;
  originationAccount: string;
  storage: string;
  burnCap: number;
}

export class ChangeToolAction {
  public readonly type = ActionType.ChangeTool;
  constructor(public payload: GenerateDeployScriptState['tool']) {}
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: GenerateDeployScriptState['entrypoint']) {}
}

export class ChangeStorageAction {
  public readonly type = ActionType.ChangeStorage;
  constructor(public payload: GenerateDeployScriptState['storage']) {}
}

type Action =
  | ChangeToolAction
  | ChangeEntrypointAction
  | ChangeStorageAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: GenerateDeployScriptState = {
  tool: Tool.TezosClient,
  entrypoint: '',
  storage: '',
  originationAccount: '',
  burnCap: 0,
};

const generateDeployScript = (
  state = DEFAULT_STATE,
  action: Action
): GenerateDeployScriptState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload
          ? DEFAULT_STATE
          : action.payload.generateDeployScript),
      };
    case ActionType.ChangeTool:
      return {
        ...state,
        tool: action.payload,
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload,
      };
    case ActionType.ChangeStorage:
      return {
        ...state,
        storage: action.payload,
      };
    default:
      return state;
  }
};

export default generateDeployScript;
