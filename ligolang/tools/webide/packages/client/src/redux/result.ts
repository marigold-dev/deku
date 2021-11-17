import { CommandType } from './types';

export enum ActionType {
  ChangeOutput = 'result-change-output',
  ChangeContract = 'result-change-contract'
}

export interface ResultState {
  command: CommandType;
  output: string;
  contract: string;
  error: boolean;
}

export class ChangeOutputAction {
  public readonly type = ActionType.ChangeOutput;
  constructor(
    public output: ResultState['output'],
    public command: ResultState['command'],
    public error: ResultState['error'] 
  ) {}
}

export class ChangeContractAction {
  public readonly type = ActionType.ChangeContract;
  constructor(
    public contract: ResultState['contract'],
    public command: ResultState['command']
  ) {}
}

type Action = ChangeOutputAction | ChangeContractAction;

const DEFAULT_STATE: ResultState = {
  command: CommandType.Compile,
  output: '',
  contract: '',
  error: false
};

const result = (state = DEFAULT_STATE, action: Action): ResultState => {
  switch (action.type) {
    case ActionType.ChangeOutput:
      return {
        ...state,
        output: action.output,
        command: action.command,
        error: action.error
      };
    case ActionType.ChangeContract:
      return {
        ...state,
        output: DEFAULT_STATE.output,
        contract: action.contract,
        command: action.command
      };
    default:
      return state;
  }
};

export default result