export enum ActionType {
  SetDefaultList = 'set-function',
  ChangeSelected = 'function-change-selected',
}

export interface CompileFunctionState {
  functionName: string;
}

const DEFAULT_STATE = {
  functionName: '',
};

export class ChangeSelectedAction {
  public readonly type = ActionType.ChangeSelected;
  constructor(public payload: CompileFunctionState['functionName']) {}
}

const compileFunction = (
  state = DEFAULT_STATE,
  action: any
): CompileFunctionState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        functionName: action.payload ? action.payload : '',
      };
    case ActionType.SetDefaultList:
      return { ...state, functionName: action.value };
    default:
      return {
        ...state,
      };
  }
};

export default compileFunction;
